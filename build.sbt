
name := "Crawler"

version := "1.0"

scalaVersion := "2.9.2"

mainClass := Some("app.crawler.Main")

libraryDependencies += "net.databinder" %% "dispatch-http" % "0.8.8"

libraryDependencies += "net.databinder" %% "dispatch-http-json" % "0.8.8"