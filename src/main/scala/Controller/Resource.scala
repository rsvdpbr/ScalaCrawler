package app.crawler.Controller

import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml.{ NodeSeq, Elem, Node, Text }
import scala.collection.mutable.{ ListBuffer, HashMap }
import nu.validator.htmlparser.sax.HtmlParser
import nu.validator.htmlparser.common.XmlViolationPolicy
import java.io.StringReader
import org.xml.sax.InputSource

import app.crawler.Model.Word

object ResourceController {

  // filtering regular expression for a tag
  val aRegExp = """^https?://.*$"""
  val aRegExpExclude = """^(javascript:|mailto:|#).*$"""

  /**
   * convert String to Node
   */
  def htmlToNode(str: String): Node = {
    if (str == null) return null // the case that error occured in Connection.getHtml
    try {
      val hp = new HtmlParser
      hp.setNamePolicy(XmlViolationPolicy.ALLOW)
      val saxer = new NoBindingFactoryAdapter
      hp.setContentHandler(saxer)
      hp.parse(new InputSource(new StringReader(str)))
      return saxer.rootElem
    } catch {
      case e => {
        println("[ERROR] " + e.toString)
        return null
      }
    }
  }

  /**
   * get page title
   */
  def getPageTitle(node: Node): String = {
    return try {
      val original = (node \\ "title")(0) toString ()
      removeTags(original)
    } catch {
      case e => { "[ERROR] Not found title tag" }
    }
  }

  /**
   * functions to get a tag
   */
  def getHrefTag(node: Node): String = node \ "@href" toString ()
  def getAllHrefTag(node: Node): NodeSeq = {
    val originalList = node \\ "a"
    val filtering = originalList filter (getHrefTag(_) matches aRegExp)
    filtering filterNot (getHrefTag(_) matches aRegExpExclude)
  }

  /**
   * remove html tags
   */
  def removeTags(str: String): String = """<.+?>""".r.replaceAllIn(str, "")
  def removeLFRF(str: String): String = """\n|\r""".r.replaceAllIn(str, "")

  /**
   * evaluate point
   */
  def evaluateText(node: Node): HashMap[Word.WordTree, HashMap[String, Any]] = {
    val text = removeTags(node.toString)
    val result = new HashMap[Word.WordTree, HashMap[String, Any]] // ("count", "point", "tag")
    val setResult = (wordTree: Word.WordTree, count: Int, point: Double) => {
      if (!result.contains(wordTree)) {
        result(wordTree) = HashMap(
          "count" -> (0: Int),
          "point" -> (0.0: Double),
          "tag" -> List())
      }
      result(wordTree)("count") = count + result(wordTree)("count").asInstanceOf[Int]
      result(wordTree)("point") = point + result(wordTree)("point").asInstanceOf[Double]
    }
    val addResultTag = (wordTree: Word.WordTree, tag: String) => {
      setResult(wordTree, 0, 0)
      result(wordTree)("tag") = result(wordTree)("tag").asInstanceOf[List[String]] :+ tag
    }
    // 全文検索によるポイント付
    // ルートを除いて、親単語がテキスト内に登場している全ての単語を評価する
    // ポイントは、「登場した回数×各単語特有の重み÷２」
    var queueHead = 0
    val wordQueue = new ListBuffer[Word.WordTree]
    Word.getChildrenTreeById(Word.getRootTree.getId).foreach(wordQueue += _)
    while (queueHead < wordQueue.length) {
      val nowRegexp = wordQueue(queueHead).getExpression.r
      val nowResult = nowRegexp.findAllIn(text).toList
      if (nowResult.length > 0) {
        val point = wordQueue(queueHead).getWeight * nowResult.length / 2
        setResult(wordQueue(queueHead), nowResult.length, point)
        Word.getChildrenTreeById(wordQueue(queueHead).getId).foreach(wordQueue += _)
      }
      queueHead += 1
    }
    // タグによるポイント付
    // タイトルや見出しには一般的な文字列も入ることは多いので、
    // WordTreeから親子関係を持つ要素が２つ以上検出された時のみ評価を行う（「lisp」「人工知能」など）
    // ただし、この例の場合、lispという文字列が必ずしもtitleタグに出てくるとは限らないため、
    // 全文検索で検出された単語を元に評価する（全文検索でtitleタグ内も検索済み）
    // 基本ポイントは、「各単語固有の重み×子の木の深さ×タグの重み」であり、
    // 親には「基本ポイント^親の木の深さ」が、子には「基本ポイント^子の木の深さ」がポイントとなる
    val specialTags = List(
      Tuple2("title", 1), // ブログタイトルなど記事との関連が薄いものが入りがちなため
      Tuple2("h1", 6), Tuple2("h2", 5), Tuple2("h3", 4),
      Tuple2("h4", 3), Tuple2("h5", 2), Tuple2("h6", 1),
      Tuple2("b", 3), Tuple2("i", 3), Tuple2("string", 3), Tuple2("em", 3))
    for (
      tag <- specialTags;
      tagRawText <- node \\ tag._1 toList
    ) {
      val tagText = removeLFRF(removeTags(tagRawText toString))
      // println(tag._1 + ": " + tagText)
      val tagResult = (for (i <- result) yield {
        val tree = i._1
        val nowResult = tree.getExpression.r.findAllIn(tagText).toList
        if (nowResult.length > 0) { tree } else { null }
      }).toList.filterNot(_ == null)
      for {
        tree <- tagResult
        parent <- result if (tree.getParent == parent._1)
      } {
        val depth = tree.getDepth
        val point = tree.getWeight * depth * tag._2
        setResult(tree, 0, scala.math.pow(point, depth))
        setResult(parent._1, 0, scala.math.pow(point, parent._1.getDepth))
        addResultTag(tree, tag._1)
        addResultTag(parent._1, tag._1)
        // println("  -> " + tree + " -> " + parent._1)
      }
    }

    // val count = (for (i <- result) yield i._2("count").asInstanceOf[Int]).reduce(_ + _)
    // val point = (for (i <- result) yield i._2("point").asInstanceOf[Double]).reduce(_ + _)
    return result
  }

}
