package app.crawler.Table

import org.scalaquery.ql._
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import basic.{ BasicTable => Table }

object Config extends Table[(String, String, Option[String])]("configs") {
  def key = column[String]("key", O PrimaryKey)
  def value = column[String]("value", O DBType "varchar(255)")
  def modified = column[Option[String]]("modified")
  def * = key ~ value ~ modified
}

