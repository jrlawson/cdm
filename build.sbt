val neo4j = "org.neo4j" % "neo4j" % "3.1.0-M04"

lazy val commonSettings = Seq(
  organization := "com.s3",
  version := "0.1.0.a",
  scalaVersion := "2.11.8"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "KnowledgeBaseSimulation",
    libraryDependencies += neo4j
  )
  
