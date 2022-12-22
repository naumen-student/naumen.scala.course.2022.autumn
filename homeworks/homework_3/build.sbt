name := "homework_3"

version := "0.1"

scalaVersion := "2.13.10"

libraryDependencies += "com.lihaoyi" %% "utest" % "0.8.1" % "test"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"

testFrameworks += new TestFramework("utest.runner.Framework")