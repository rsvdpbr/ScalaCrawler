
name := "Crawler"

version := "1.0"

scalaVersion := "2.9.2"

mainClass := Some("app.crawler.Main")

libraryDependencies += "net.databinder" %% "dispatch-http" % "0.8.8"

libraryDependencies += "net.databinder" %% "dispatch-http-json" % "0.8.8"

libraryDependencies += "org.scalaquery" % "scalaquery_2.9.0-1" % "0.9.5"

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.18"

libraryDependencies += "net.debasishg" % "sjson_2.9.1" % "0.15"