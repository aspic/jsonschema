organization := "no.mehl"

name := "jsonschema"

description := "Extension for Argonaut to generate json schema"
version := "0.1.0"
scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "io.argonaut"   %% "argonaut"  % "6.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)