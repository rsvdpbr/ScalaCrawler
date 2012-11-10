package app.crawler.Model

import org.scalaquery.ql._
import org.scalaquery.ql.extended.MySQLDriver.Implicit._
import org.scalaquery.simple.StaticQuery._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.extended.{ ExtendedTable => Table }

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.util.control.Breaks._

object Word extends Table[(Int, Int, String, String, Float, Option[String], Option[String])]("words") {

  // define fields
  def id = column[Int]("id", O PrimaryKey)
  def parent_id = column[Int]("parent_id")
  def word = column[String]("display_word", O DBType "varchar(256)")
  def regexp = column[String]("word_expression", O DBType "varchar(512)")
  def weight = column[Float]("weight")
  def created = column[Option[String]]("created")
  def modified = column[Option[String]]("modified")
  def * = id ~ parent_id ~ word ~ regexp ~ weight ~ created ~ modified
  // define variables
  var wordTree: WordTree = null
  // define type
  type WordTreeUnit = HashMap[String, Any]

  // get initial query
  def getInitialQuery(): Query[Word.type] = {
    return this
  }

  // get word-tree
  def setupWordTree(rootWord: String = "*"):Unit = {
    // fetch data from database
    val query = for {
      wp <- getInitialQuery()
      wc <- getInitialQuery() orderBy wp.id if wp.id === wc.parent_id
    } yield wp.id ~ wp.word ~ wp.regexp ~ wp.weight ~ wc.id ~ wc.word ~ wc.regexp ~ wc.weight
    val result = AppModel.execute.withSession { query.list }
    // format data to hashmap list
    val data = new ListBuffer[HashMap[String, HashMap[String, Any]]]
    result.foreach(item => {
      data += HashMap(
        "parent" -> HashMap(
          "id" -> item._1,
          "word" -> item._2,
          "expression" -> item._3,
          "weight" -> item._4),
        "self" -> HashMap(
          "id" -> item._5,
          "word" -> item._6,
          "expression" -> item._7,
          "weight" -> item._8))
    })
    // create word tree
    val root = new WordTree()
    data.foreach(item => {
      root.appendChild(item("parent"), item("self"))
    })
    wordTree = root
  }

  // access to WordTree object
  def getRootTree(): WordTree = wordTree
  def getPartialTreeById(id: Int): WordTree = wordTree.getPartialTreeById(id)
  def getPartialTreeByWord(word: String): WordTree = wordTree.getPartialTreeByWord(word)

  class WordTree(_self: WordTreeUnit = null) {
    val self = if (_self != null) { _self } else { HashMap("id" -> 0, "word" -> "ROOT") }
    val children = new ListBuffer[(WordTree, ListBuffer[Int])]

    def appendChild(_parent: WordTreeUnit, _self: WordTreeUnit): Boolean = {
      if (self == _parent) {
        // _parentがselfならば
        // _selfはselfの子となる
        children.append((new WordTree(_self), new ListBuffer[Int]))
      } else {
        // _parentがselfではないならば
        // _parentのIDがchildrenのself又はchildren以下に存在するならば、
        // 再帰的に、children内の_parentに_selfを追加する
        breakable {
          children.foreach(child => {
            if (child._1.self("id") == _parent("id") || child._2.contains(_parent("id"))) {
              child._2.append(_self("id").asInstanceOf[Int])
              child._1.appendChild(_parent, _self)
              break
            }
          })
          // _parentがchildren以下に存在しなければ、
          // selfの子に_parentを追加し、_parentの子にselfを追加する
          children.append((new WordTree(_parent), new ListBuffer[Int]))
          children.last._1.appendChild(_parent, _self)
          children.last._2.append(_self("id").asInstanceOf[Int])
        }
      }
      return true
    }

    /**
     * override toString() to show number of children
     */
    override def toString(): String = this.toString(0)
    def toString(depth: Int): String = {
      // var string = "[%03d: %s] has %d children".format(
      var string = "[%03d: %s] (%s)".format(
        self("id"), self("word"), if (children.length > 0) { children.length } else { "-" })
      children.foreach(child => {
        string += "\n" + "-" * (depth + 1) * 2 + "> " + child._1.toString(depth + 1)
      })
      return string
    }

    /**
     * search partial tree by id or word
     */
    def getPartialTreeById(id: Int): WordTree = {
      if (self("id") == id) return this
      children.foreach(child => {
        if (child._1.self("id") == id) return child._1
        else if (child._2.contains(id)) return child._1.getPartialTreeById(id)
      })
      return null
    }
    def getPartialTreeByWord(word: String): WordTree = {
      if (self("word") == word) return this
      children.foreach(child => {
        if (child._1.self("word") == word) return child._1
        else {
          val tmp = child._1.getPartialTreeByWord(word)
          if (tmp != null) return tmp
        }
      })
      return null
    }

  }
}

