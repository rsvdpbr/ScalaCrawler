package app.crawler.Controller

import java.net.URLEncoder
import scala.collection.mutable.{ ListBuffer }
import scala.xml.{ NodeSeq, Elem, Node, Text }
import dispatch._

import app.crawler.Controller.{ ResourceController => Resource }
import app.crawler.Model.Config

object ConnectionController {

  private val urlCache = new ListBuffer[String]
  private val targetQueue = new ListBuffer[(String, Int)]
  private var lastErrorMessage = ""

  /**
   * add url to member list
   */
  def addUrlCache(url: String): Unit = urlCache += url
  def addTargetQueue(url: String, ttl: Int): Unit = targetQueue += Tuple2(url, ttl)

  /**
   * getter functions
   */
  def getUrlFromTargetQueueById(id: Int): String = {
    return if (id < targetQueue.length) { targetQueue(id)._1 } else { null }
  }
  def getTTLFromTargetQueueById(id: Int): Int = {
    return if (id < targetQueue.length) { targetQueue(id)._2 } else { 0 }
  }

  /**
   * validate and make error message for adding url
   */
  def getErrorMessage = lastErrorMessage
  def validateForAdding(url: String): Boolean = {
    lastErrorMessage = validateForAddingWithString(url)
    return lastErrorMessage.isEmpty
  }
  def validateForAddingWithString(url: String): String =
    if (urlCache.contains(url)) "Already cached"
    else ""

  /**
   * get Html data from Url
   */
  def getHtml(target: String, encode: String = "UTF-8"): String = {
    val h = new Http
    return try {
      val req = url(target) >\ encode
      h(req as_str) // possible to throw MalformedInputException
    } catch {
      case e => {
        println("[ERROR] " + e.toString.split("\n")(0))
        null
      }
    }
  }

  /**
   * remove hash
   */
  def removeHash(str: String): String = """#.?""".r.replaceAllIn(str, "")

  def initSearch(): Unit = {
    // init url lists
    urlCache.clear()
    targetQueue.clear()
    // get urls from yahoo search api and register to list
    getUrlsBySearchWord().foreach(url => {
      addUrlCache(url)
      addTargetQueue(url, -1)
    })
  }

  /**
   * return information of page about word with yahoo search api
   */
  private def getUrlsBySearchWord(page: Int = 1): List[String] = {
    // set variables and import break library
    val query = URLEncoder.encode(Config.getByKey("search-start-words"), "utf-8")
    val url = "http://search.yahooapis.jp/WebSearchService/V2/webSearch" +
      "?appid=" + Config.getByKey("yahoo-api-id") + // apiID
      "&query=" + query + // 検索文字列
      "&type=all" + // allだと全クエリを含む検索結果、anyだと一部を含む
      "&results=20" + // 結果の数。20がマックス
      "&start=" + (page * 20 + 1) + // 結果の先頭位置
      "&format=html" // 検索するファイルの種類
    // request for search result until results returned or 20 requests posted
    var node: Node = null
    var counter = 20;
    while (counter > 0) {
      node = Resource.htmlToNode(this.getHtml(url))
      if (node != null) {
        counter = 0
      }
      counter -= 1
    }
    // return results if there is no error
    var result: List[String] = null
    if (node != null) {
      result = (
        for (i <- node \\ "result") yield Resource.removeTags(i \\ "clickurl" toString)).toList
    } else {
      result = List()
    }
    return result
  }

}

