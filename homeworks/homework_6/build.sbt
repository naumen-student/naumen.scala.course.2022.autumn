name := "homework_6"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % Test
testFrameworks += new TestFramework("utest.runner.Framework")
