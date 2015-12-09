enablePlugins(ScalaJSPlugin)

sbtPlugin := true

name := "ScalaCZML"

organization := "com.kodekutters"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.5.0-M1"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")