name := """server"""
organization := "com.example"

version := "1.0-SNAPSHOT"

lazy val localModule = RootProject(file("/Users/prokopovapa/Desktop/sanskrit-lang"))

lazy val root = (project in file(".")).enablePlugins(PlayScala).dependsOn(localModule)

scalaVersion := "3.3.1"

libraryDependencies += guice
libraryDependencies += "org.scalatestplus.play" %% "scalatestplus-play" % "7.0.0" % Test
     


// Adds additional packages into Twirl
//TwirlKeys.templateImports += "com.example.controllers._"

// Adds additional packages into conf/routes
// play.sbt.routes.RoutesKeys.routesImport += "com.example.binders._"