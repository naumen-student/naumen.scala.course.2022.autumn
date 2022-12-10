name := "homework_7"

version := "0.1"

scalaVersion := "2.13.10"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test"

scalacOptions ++= Seq("-language:higherKinds")

testFrameworks += new TestFramework("utest.runner.Framework")