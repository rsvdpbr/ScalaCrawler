package app.crawler.Model

import org.scalaquery.session.{ Database, Session }

object AppModel {

  // connection with database
  val db = Database.forURL(
    "jdbc:mysql://localhost/database_name",
    driver = "com.mysql.jdbc.Driver",
    user = "username",
    password = "password"
  )

  // called by other specific models to execute query
  def execute(): Database = return db

}
