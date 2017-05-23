organization := "no.mehl"

name := "argonaut-swagger"

description := "Tid er penger"
version := "0.1.0"
scalaVersion := "2.12.2"
// resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  "io.argonaut"   %% "argonaut"  % "6.2",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)