import Dependencies._
import sbt.Keys.initialCommands

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "com.iuriisusuk",
      scalaVersion := "2.12.1",
      version := "0.1.0-SNAPSHOT"
    )),
  name := "iuvi",
  libraryDependencies ++= Seq(
    "com.chuusai" %% "shapeless" % "2.3.2",
    "org.typelevel" %% "cats" % "0.9.0",
    "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.78",
    scalaTest % Test
  )
)
