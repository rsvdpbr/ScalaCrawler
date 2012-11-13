package app.crawler.Model

import org.scalaquery.ql._
import org.scalaquery.ql.extended.MySQLDriver.Implicit._
import org.scalaquery.simple.StaticQuery._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.extended.{ ExtendedTable => Table }

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.util.control.Breaks._

object Word extends Table[(Int, Int, String, String, Double, Option[String], Option[String])]("words") {

  // define fields
  def id = column[Int]("id", O PrimaryKey)
  def parent_id = column[Int]("parent_id")
  def word = column[String]("display_word", O DBType "varchar(256)")
  def regexp = column[String]("word_expression", O DBType "varchar(512)")
  def weight = column[Double]("weight")
  def created = column[Option[String]]("created")
  def modified = column[Option[String]]("modified")
  def * = id ~ parent_id ~ word ~ regexp ~ weight ~ created ~ modified
  // define variables
  var wordTree: WordTree = null

  // get initial query
  def getInitialQuery(): Query[Word.type] = {
    return this
  }

  // get word-tree
  def setupWordTree(rootWord: String = "*"): Unit = {
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
  def getChildrenTreeById(id: Int): List[WordTree] = {
    val parent = getPartialTreeById(id)
    return (for (child <- parent.children) yield child._1) toList
  }

  class WordTree(_self: HashMap[String, Any] = null) {
    val self = if (_self != null) { _self } else { HashMap("id" -> 0, "word" -> "ROOT") }
    val children = new ListBuffer[(WordTree, ListBuffer[Int])]
    type WordTreeUnit = HashMap[String, Any]

    def getId(): Int = self("id").asInstanceOf[Int]
    def getWord(): String = self("word").asInstanceOf[String]
    def getWeight(): Double = if (self.contains("weight")) {
      self("weight").asInstanceOf[Double]
    } else { 0.0 }
    def getExpression(): String = if (self.contains("expression")) {
      "(?i)" + self("expression").asInstanceOf[String]
    } else { "" }

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
    override def toString(): String = "[%03d: %s] (%s)".format(
      self("id"), self("word"), if (children.length > 0) { children.length } else { "-" })
    def dump(): String = this.dump(0)
    def dump(depth: Int): String = {
      var string = toString
      children.foreach(child => {
        string += "\n" + "-" * (depth + 1) * 2 + "> " + child._1.dump(depth + 1)
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
    def getParent(): WordTree = {
      val id = getId
      var queueHead = 0
      val queue = new ListBuffer[(WordTree, WordTree)] // (tree, parent-tree)
      queue += Tuple2(getRootTree, null)
      while (queueHead < queue.length) { // 幅優先探索
        val tree = queue(queueHead)._1
        queueHead += 1
        if (this == tree) { return queue(queueHead - 1)._2 }
        tree.children.foreach(
          child => if (child._2.contains(id) || child._1.getId == id) {
            queue += Tuple2(child._1, tree)
          })
      }
      return null
    }
    def getDepth: Int =  getDepth()
    def getDepth(root: WordTree = getRootTree): Int = {
      val id = getId
      var depth = 0 // nullが来るたびにdepthを一つ増やす
      var queueHead = 0
      val queue = ListBuffer(root, null)
      while (queueHead < queue.length) { // 幅優先探索
        breakable {
          val tree = queue(queueHead)
          queueHead += 1
          if (tree == null) { // null処理：終了条件の判定とdepthインクリメント
            if (queue(queueHead - 2) == null) {
              queueHead = queue.length
              depth = -1
            } else {
              depth += 1
              queue += null
            }
            break
          }
          if (id == tree.getId) { // 目的の木を発見
            queueHead = queue.length
            break
          }
          tree.children.foreach(
            child => if (child._2.contains(id) || child._1.getId == id) {
              queue += child._1
            })
        } // end of break scope
      }
      return depth
    }

  }
}

