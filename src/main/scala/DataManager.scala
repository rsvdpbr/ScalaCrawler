package app.crawler

import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.util.control.Breaks._
import org.scalaquery.simple.StaticQuery._
import org.scalaquery.session.{ Database, Session }
import org.scalaquery.session.Database.threadLocalSession

class DataManager {

  // connection with database
  val db = Database.forURL(
    "jdbc:mysql://localhost/crawler",
    driver = "com.mysql.jdbc.Driver",
    user = "w-leads",
    password = "4MACTY")

  // get config value
  def getByKey(key: String): String = {
    val result = db.withSession {
      queryNA[(String)]("SELECT value FROM configs WHERE `key` = '" + key + "'").list
    }
    result(0)
  }

  // get word-tree
  def getWordTree(rootWord: String = "*"): Unit = {
    // fetch data from database
    val result = db.withSession {
      queryNA[(Int, String, String, Float, Int, String, String, Float)]("""
		SELECT
		  parent.id AS parent_id,
		  parent.display_word AS parent_display_word,
		  parent.word_expression AS parent_word_expression,
		  parent.weight AS parent_weight,
		  child.id AS child_id,
		  child.display_word AS child_display_word,
		  child.word_expression AS child_word_expression,
		  child.weight AS child_weight
		FROM words AS parent
		INNER JOIN words AS child ON child.parent_id = parent.id
		ORDER BY parent.id
		""").list
    }
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
    println()
    println(root)

  }

}

class WordTree(_self: HashMap[String, Any] = null) {
  type WordTreeUnit = HashMap[String, Any]
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
    var string = "[%03d: %s] has %d children" format (self("id"), self("word"), children.length)
    children.foreach(child => {
      string += "\n" + "-" * (depth + 1) * 2 + "> " + child._1.toString(depth + 1)
    })
    return string
  }

}
