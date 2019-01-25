name := "tetris"

version := "0.1"

scalaVersion := "2.12.6"

libraryDependencies  ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "jline" % "jline" % "2.14.6"
)

libraryDependencies += "org.apache.httpcomponents" % "httpclient" % "4.5.6"
libraryDependencies +=  "org.scalaj" %% "scalaj-http" % "2.4.1"
libraryDependencies += "com.google.code.gson" % "gson" % "2.3.1"