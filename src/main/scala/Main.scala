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
    """機械学習""" -> ListBuffer(
      """(機械学習|(m|M)achine (l|L)earning)""",
      """(入門|(t|T)utorial)""",
      """(文系|分かる)""",
      """(プログラ(ム|ミング)|(p|P)rogram(ing)?)""",
      """(自然言語)""",
      """(教師)""",
      """(l|L)isp""",
      """(c|C)lojure""",
      """(p|P)ython"""),
    """(l|L)isp""" -> ListBuffer(
      """(入門|(t|T)utorial)""",
      """(S式|S ?(e|E)xpression)""",
      """(データベース|(d|D)ata(b|B)ase)""",
      """(人工知能|AI)""",
      """(末尾再帰|(t|T)ail (r|R)ecursion)""",
      """(パース|解析|解釈|parse)""",
      """(自然言語)""",
      """(サンプル|(s|S)ample)""",
      """(機械学習|(m|M)achine (l|L)earning)"""),
    """(c|C)lojure""" -> ListBuffer(
      """(入門)""",
      """(再帰|recursive)""",
      """(末尾再帰|(t|T)ail (r|R)ecursion)""",
      """(DSL|(d|D)omain( |-)?(s|S)pecific (l|L)anguage|ドメイン固有言語)""",
      """(XML|Xml|xml)""",
      """(パース|解析|解釈|parse)""",
      """(自然言語)""",
      """(機械学習|(m|M)achine (l|L)earning)"""))
  // the word to call yahoo search api with
  val startWords = "機械学習"
  // step
  var step = 2000
  // filtering regular expression for a tag
  val aRegExp = """^https?://.*$"""
  val aRegExpExclude = """^(javascript:|mailto:|#).*$"""

  // list for caching url
  val urlCache = new ListBuffer[String]
  // queque for url
  var queueCounter = 0
  val targetQueue = new ListBuffer[String]
  // list for data
  type Data = (Int, String, String, Node, Double) // (ID, URL, Title, HtmlNode, Point)
  val dataList = new ListBuffer[Data]
  // application id of yahoo apis
  val apiId = "1lrHQU.xg66Lo7X6gX8LGae3czjpWQlO90e5acikqxEUQDnsrtMlILwHWDXHsPr0jAE-"

  /**
   * main function
   */
  def main(): Unit = {
    // set urls
    searchWord(startWords).foreach(n => {
      urlCache += n._3
      targetQueue += n._3
    })
    // main while loop
    val mycontinue = new Breaks()
    import mycontinue.{ break => continue, breakable => continuable }
    while (queueCounter < step) {
      continuable {
        // get next url from targetQueue
        val target: String = try {
          targetQueue(queueCounter)
        } catch {
          case e => {
            println("\n[END OF QUEUE] " + e.toString)
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
          case e => {
            println("[ERROR] " + e.toString.split("\n")(0))
            step += 1
            continue
            null
          }
        }
        // print page title
        val title: String = try {
          val original = (node \\ "title")(0) toString ()
          removeTags(original)
        } catch {
          case e => "[ERROR] Not fond title tag"
        }
        println("  -> title : " + title)
        // get point and show
        val point = getPoint(node.toString)
        dataList += Tuple5(dataList.length, target, title, node, point)
        println("  -> point: " + point)
        // get list of a tag from html if point is more than 0
        if (point > 0) {
          var aTag = {
            val originalList = node \\ "a"
            val filtering = originalList filter (_ \ "@href" toString () matches aRegExp)
            filtering filterNot (_ \ "@href" toString () matches aRegExpExclude)
          }
          aTag.foreach(n => {
            val url = removeHash(n \ "@href" toString ())
            if (urlCache.contains(url)) {
              println("    -> Already cached : " + url)
            } else {
              println("    -> " + url)
              urlCache += url
              targetQueue += url
            }
          })
        }
      }
      println()
      queueCounter += 1
    }
    // sort by point
    val sortedList = dataList.toSeq.sortWith(_._5 > _._5)
    for (i <- 0 to sortedList.length - 1) {
      println("[%04d] (" + sortedList(i)._5 + ") %s" format (sortedList(i)._1, sortedList(i)._2))
      println("    -> " + sortedList(i)._3)
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
   * remove hash
   */
  def removeHash(str: String): String = {
    """#.?""".r.replaceAllIn(str, "")
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
        removeTags(i \\ "clickurl" toString))
    }
    results
  }
}

