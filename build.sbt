ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "Sanskrit",
    scalacOptions ++= Seq(
      "-source:future"
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-parse" % "0.3.9",
      "org.scalatest" %% "scalatest" % "3.2.17" % "test"
    )
  )
