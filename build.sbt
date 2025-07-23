ThisBuild / version := "0.1.1-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "Ants",
    version := "1.1",
    scalaVersion := "2.13.15",
    mainClass in Compile := Some("AntAlgorithmSolver")
  )

libraryDependencies += "com.github.scopt" %% "scopt" % "4.1.0"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.2.0"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.18"

unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "resources"

enablePlugins(ScalafmtPlugin)

ThisBuild / scalafmtOnCompile := true

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case PathList("/src/main/resources/ATT48.txt")    => MergeStrategy.concat
  case x => MergeStrategy.first
}