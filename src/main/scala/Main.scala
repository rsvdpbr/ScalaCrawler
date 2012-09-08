package app.crawler

import java.io.StringReader
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
  // val

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
            """</?(title|TITLE)>""".r.replaceAllIn(original, "")
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
      }
      println()
      queueCounter += 1
    }
    for (i <- 0 to targetQueue.length - 1) {
      println("[%04d] %s" format (i, targetQueue(i)))
    }
    // targetQueue.foreach(e => println(e))
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
}

