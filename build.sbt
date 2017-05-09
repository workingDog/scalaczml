
name := "scalaczml"

version := (version in ThisBuild).value

organization := "com.github.workingDog"

scalaVersion := "2.11.11"

crossScalaVersions := Seq("2.11.11", "2.12.2")

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.5.14"

javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

scalacOptions ++= Seq( "-unchecked", "-deprecation",  "-feature"  )

homepage := Some(url("https://github.com/workingDog/scalaczml"))

licenses := Seq("Apache 2" -> url("http://www.apache.org/licenses/LICENSE-2.0"))

