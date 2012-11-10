package app.crawler.Model

import org.scalaquery.ql._
import org.scalaquery.ql.extended.MySQLDriver.Implicit._
import org.scalaquery.simple.StaticQuery._
import org.scalaquery.session.Database.threadLocalSession
import org.scalaquery.ql.extended.{ ExtendedTable => Table }

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

  // get initial query regarding set_name
  def getInitialQuery(): Query[Config.type] = {
    return useSet match {
      case null => this
      case _: String => this.where(_.set_name === useSet)
    }
  }

  // get config value by key
  def getByKey(key: String): String = {
    val query = getInitialQuery.where(_.key === key) map { config => config.value }
    val result = AppModel.execute.withSession { query.first }
	return result
  }

}

