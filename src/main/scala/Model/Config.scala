package app.crawler.Model

import org.scalaquery.ql._
import org.scalaquery.ql.basic.BasicDriver.Implicit._
import org.scalaquery.simple.StaticQuery._
import org.scalaquery.session.Database.threadLocalSession
import basic.{ BasicTable => Table }

object Config extends Table[(Int, String, String, String, Option[String])]("configs") {

  // define fields
  def id = column[Int]("id", O PrimaryKey)
  def set_name = column[String]("set_name", O DBType "varchar(255)")
  def key = column[String]("key", O NotNull)
  def value = column[String]("value", O DBType "varchar(255)")
  def modified = column[Option[String]]("modified")
  def * = id ~ set_name ~ key ~ value ~ modified
  // define variables
  var useSet: String = null

  // changeSnetting
  def changeSetting(name: String): Unit = { useSet = name }
  // get where query regarding set_name
  def getSettingQuery(): String = {
    return useSet match {
      case null => "1 = 1"
      case _: String => "`set_name` = '" + useSet + "'"
    }
  }

  // get config value
  def getByKey(key: String): String = {
    val result = AppModel.execute.withSession {
      queryNA[(String)](
		"""SELECT `value` FROM `configs` WHERE `key` = '""" + key + """' AND """ + getSettingQuery()
	  ).list
    }
    result(0)
  }

}

