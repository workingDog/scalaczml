
name := "ScalaCZML"

organization := "com.kodekutters"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.typesafe.play" % "play-json_2.11" % "2.5.0-M1",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")
