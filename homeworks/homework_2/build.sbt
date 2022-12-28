name := "Lecture2"

version := "0.1"

scalaVersion := "2.13.9"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test"

testFrameworks += new TestFramework("utest.runner.Framework")