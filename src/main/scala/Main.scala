package app.crawler

import java.io.StringReader
import java.net.URLEncoder
import scala.math._
import scala.xml.{ NodeSeq, Elem, Node, Text }
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.util.control.Breaks
import dispatch._
import nu.validator.htmlparser.sax.HtmlParser
import nu.validator.htmlparser.common.XmlViolationPolicy
import org.xml.sax.InputSource

import org.scalaquery.session.Database
import org.scalaquery.session.Database.threadLocalSession

/**
 * Main object
 */
object Main extends App {

  // search words
  val searchWords = HashMap(
    """(c|C)lojure""" -> ListBuffer(
      """(XML|Xml|xml)""",
      """(パース|parse|解析|解釈)""",
      """(機械学習|(M|m)achine\s?study)"""
	),
	"""(s|S)cala""" -> ListBuffer(
	  """((c|C)ollection|コレクション)"""
	)
  )
  // search start url  "http://www.wasedasai.net/2012/"
  // val startTarget = "http://clojure.org/"
  var startTarget = "http://d.hatena.ne.jp/tototoshi/20111230/1325239770"
  // step
  val step = 1000
  // filtering regular expression for a tag
  val aRegExp = """^https?://.*$"""
  val aRegExpExclude = """^(javascript:|mailto:|#).*$"""

  // list for caching url
  val urlCache = new ListBuffer[String]
  urlCache += startTarget
  // queque for url
  var queueCounter = 0
  val targetQueue = new ListBuffer[String]
  targetQueue += startTarget
  // list for data
  type Data = (Int, String, Node, Double) // (ID, URL, HtmlNode, Point)
  val dataList = new ListBuffer[Data]
  // application id of yahoo apis
  val apiId = "1lrHQU.xg66Lo7X6gX8LGae3czjpWQlO90e5acikqxEUQDnsrtMlILwHWDXHsPr0jAE-"

  /**
   * main function
   */
  def main(): Unit = {
    val mycontinue = new Breaks()
    import mycontinue.{ break => continue, breakable => continuable }
    while (queueCounter < step) {
      continuable {
        // get next url from targetQueue
        val target: String = try {
          targetQueue(queueCounter)
        } catch {
          case e => {
            println("\n" + e.toString)
            queueCounter = step
            continue
            null
          }
        }
        // get html data from url
        println("[" + queueCounter + "] " + target)
        val node: Node = try {
          getHtmlNode(target)
        } catch {
          case e => println("[ERROR]" + e.toString); continue; null
        }
        // print page title
        try {
          val title: String = {
            val original = (node \\ "title")(0) toString ()
            removeTags(original)
            // """</?(title|TITLE)>""".r.replaceAllIn(original, "")
          }
          println("  -> title : " + title)
        } catch {
          case e => println("[ERROR] Not fond title tag : " + e.toString)
        }
        // get list of a tag from html
        var aTag = {
          val originalList = node \\ "a"
          val filtering = originalList filter (_ \ "@href" toString () matches aRegExp)
          filtering filterNot (_ \ "@href" toString () matches aRegExpExclude)
        }
        aTag.foreach(n => {
          val url = n \ "@href" toString ()
          if (urlCache.contains(url)) {
            println("    -> Already cached : " + url)
          } else {
            println("    -> " + url)
            urlCache += url
            targetQueue += url
          }
        })
        // get point and show
        val point = getPoint(node.toString)
        dataList += Tuple4(dataList.length, target, node, point)
        println("  -> point: " + point)
      }
      println()
      queueCounter += 1
    }
    // sort by point
    val sortedList = dataList.toSeq.sortWith(_._4 > _._4)
    for (i <- 0 to sortedList.length - 1) {
      println("[%04d] (" + sortedList(i)._4 + ") %s" format (sortedList(i)._1, sortedList(i)._2))
    }
  }
  main()

  /**
   * get Html data from Url
   */
  def getHtmlNode(target: String, encode: String = "UTF-8"): Node = {
    val h = new Http
    val req = url(target) >\ encode
    val html = h(req as_str)
    toNode(html)
  }

  /**
   * convert String to Node
   */
  private def toNode(str: String): Node = {
    val hp = new HtmlParser
    hp.setNamePolicy(XmlViolationPolicy.ALLOW)
    val saxer = new NoBindingFactoryAdapter
    hp.setContentHandler(saxer)
    hp.parse(new InputSource(new StringReader(str)))
    saxer.rootElem
  }

  /**
   * remove html tags
   */
  def removeTags(str: String): String = {
    """<.+?>""".r.replaceAllIn(str, "")
  }

  /**
   * get point from text match
   */
  def getPoint(text: String): Double = {
    val pointList = for ((root, words) <- searchWords; w <- words) yield {
      val rootLength = root.r.findAllIn(text).toList.length
      val matchLength = w.r.findAllIn(text).toList.length
      if (matchLength * rootLength > 0) 1 else 0
    }
    pointList.reduce((a, b) => a + b)
  }

  /**
   * search about words with yahoo api
   */
  type searchResultUnit = (String, String, String) // (Title, Summary, URL)
  def searchWord(word: String, page: Int = 1): ListBuffer[searchResultUnit] = {
    val query = URLEncoder.encode(word, "utf-8")
    val url = "http://search.yahooapis.jp/WebSearchService/V2/webSearch" +
      "?appid=" + apiId + // apiID
      "&query=" + query + // 検索文字列
      "&type=all" + // allだと全クエリを含む検索結果、anyだと一部を含む
      "&results=20" + // 結果の数。20がマックス
      "&start=" + (page * 20 + 1) + // 結果の先頭位置
      "&format=html" // 検索するファイルの種類
    val node = getHtmlNode(url)
    val results = new ListBuffer[searchResultUnit]
    for (i <- node \\ "result") {
      results += Tuple3(
        removeTags(i \\ "title" toString),
        removeTags(i \\ "summary" toString),
        removeTags(i \\ "url" toString))
    }
    results
  }
}

