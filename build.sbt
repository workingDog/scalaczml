
name := "scalaczml"

version := (version in ThisBuild).value

organization := "com.github.workingDog"

scalaVersion := "2.11.8"

crossScalaVersions := Seq("2.11.8")

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.5.3",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "ch.qos.logback" % "logback-classic" % "1.1.3")

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

scalacOptions ++= Seq( "-unchecked", "-deprecation",  "-feature"  )

homepage := Some(url("https://github.com/workingDog/scalaczml"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

