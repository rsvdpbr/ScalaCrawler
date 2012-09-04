package app.crawler

import java.io.StringReader
import scala.xml.{ NodeSeq, Elem, Node, Text }
import scala.xml.parsing.NoBindingFactoryAdapter
import dispatch._
import nu.validator.htmlparser.sax.HtmlParser
import nu.validator.htmlparser.common.XmlViolationPolicy
import org.xml.sax.InputSource

/**
 * Main object
 */
object Main extends App {

  val target = "http://www.wasedasai.net/2012/"
  
  val node = getHtmlNode(target)
  val aNode = node \\ "a" filterNot (_ \ "@href" toString () matches """^(javascript:|#).*$""")
  
  aNode.foreach(n => println(n \ "@href"))

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
