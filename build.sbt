val neo4j = "org.neo4j" % "neo4j" % "3.1.0-M04"

val neo4jScalaWrapper = "eu.fakod" % "neo4j-scala_2.10" % "0.3.3"

val scalaLogging = "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11" % "2.1.2"

// "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",  
//"com.typesafe" %% "scalalogging-slf4j" % "1.0.1",  

//val slf4j "org.slf4j" % "slf4j-api" % "1.7.1"

//"org.slf4j" % "log4j-over-slf4j" % "1.7.1",  // for any java classes looking for this  

val logback = "ch.qos.logback" % "logback-classic" % "1.0.3"  


lazy val commonSettings = Seq(
  organization := "com.s3",
  version := "0.1.0.a",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings (
    name := "cdm",
    libraryDependencies ++= Seq(
      neo4j, scalaLogging, logback
    )
  )
  
