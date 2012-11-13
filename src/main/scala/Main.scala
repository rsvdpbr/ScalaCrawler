package app.crawler

import scala.math._
import scala.xml.Node
import scala.collection.mutable.{ ListBuffer, HashMap }
import scala.util.control.Breaks._

import app.crawler.Controller.{
  ConnectionController => Connection,
  ResourceController => Resource
}
import app.crawler.Model.{ Config, Word }

/**
 * Main object
 */
object Main extends App {

  // list for data
  type Data = (Int, String, String, Node, Double) // (ID, URL, Title, HtmlNode, Point)
  val dataList = new ListBuffer[Data]

  def testWord(): Unit = {
    Word.setupWordTree()
    println(Word.getRootTree.dump)
    println(Word.getPartialTreeById(15))
    println(Word.getPartialTreeByWord("XML"))
    Word.getChildrenTreeById(0).foreach(n => println(n.getExpression + " : " + n))
  }
  def testMainLoop(): Unit = {
    Word.setupWordTree()
    var url = "http://d.hatena.ne.jp/tailisland/20120808/1344428525"
    url = "http://d.hatena.ne.jp/naokirin/20111211/1323493110"
	url = "http://e-arrows.sakura.ne.jp/assets_c/2010/12/l5-countdown-166.html"
    for (
      url <- List(
        "http://www4.atwiki.jp/emaxser/pages/21.html",
        "http://www4.atwiki.jp/emaxser/pages/19.html",
        "http://valvallow.blogspot.com/2010/09/fib.html",
        "http://valvallow.blogspot.com/2010/05/tss-beglis-box-all-evlis.html",
        "http://books.shoeisha.co.jp/book/b101671.html",
        "http://ypsilonbox.blogspot.com/2011/06/clojure-memoize.html",
        "http://d.hatena.ne.jp/uehaj/20100904/1283553508",
        "http://e-arrows.sakura.ne.jp/2010/08/is-lisp-really-has-too-many-parenthesis.html",
        "http://cx4a.blogspot.com/2011/09/clojure-13.html",
        "http://valvallow.blogspot.com/2010/03/clojure-2.html",
        "http://valvallow.blogspot.com/2010/03/blog-post_12.html",
        "http://valvallow.blogspot.com/2010/02/google-app-engine-clojure.html",
        "http://hiroftp.blogspot.com/2010/05/clojure2-3.html",
        "http://sassylog.blogspot.com/2010/03/androidandroidweb.html",
        "http://ratememo.blog17.fc2.com/blog-category-73.html",
        "http://e-arrows.sakura.ne.jp/",
        "http://e-arrows.sakura.ne.jp/2010/08/is-lisp-really-has-too-many-parenthesis.html#comments",
        "http://e-arrows.sakura.ne.jp/2010/08/is-lisp-really-has-too-many-parenthesis.html#trackbacks",
        "http://e-arrows.sakura.ne.jp/lisp/",
        "http://e-arrows.sakura.ne.jp/2010/09/super-read-macro.html")
    ) {
	  println
	  println
	  println(url)
      val node = Resource.htmlToNode(Connection.getHtml(url))
      println(Resource.evaluateText(node))
    }
  }
  /**
   * main function
   */
  def main(): Unit = {
    return basicSearch
  }
  def basicSearch(): Unit = {
    // set initial urls with yahoo search api
    Connection.initSearch()
    // set word tree for evaluating point
    Word.setupWordTree()
    // main while loopx
    var queueCounter = 0
    var step = Config.getByKey("search-step-number").toInt
    while (queueCounter < step) {
      breakable {
        // get next url from targetQueue
        val target = Connection.getUrlFromTargetQueueById(queueCounter)
        if (target == null) {
          queueCounter = step
          break
        }
        // get html data from url
        println("[" + queueCounter + "] " + target)
        val node = Resource.htmlToNode(Connection.getHtml(target))
        if (node == null) {
          step += 1
          break
        }
        // print page title
        val title = Resource.getPageTitle(node)
        println("  -> title : " + title)
        // get point and show
        val eval = Resource.evaluateText(node)
        val point = if (eval.size > 0) {
          (for (i <- eval) yield i._2("point").asInstanceOf[Double]).reduce(_ + _)
        } else {
          0
        }
        println("  -> point: " + point)
        dataList += Tuple5(dataList.length, target, title, node, point)
        // get list of a tag from html if point is more than 0
        if (point > 0) {
          Resource.getAllHrefTag(node).foreach(n => {
            val url = Connection.removeHash(Resource.getHrefTag(n))
            if (Connection.addUrlIfNotContained(url)) {
              println("    -> " + url)
            } else {
              println("    -> Already cached : " + url)
            }
          })
        }
      } // end of breakable scope
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

}
