ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.15"

lazy val root = (project in file("."))
  .settings(
    name := "Ants",
    version := "1.0",
    scalaVersion := "2.13.15",
    mainClass in Compile := Some("AntAlgorithmSolver")
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15" % Test

unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "resources"

enablePlugins(ScalafmtPlugin)

ThisBuild / scalafmtOnCompile := true

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs @ _*) => MergeStrategy.discard
  case PathList("/src/main/resources/ATT48.txt")    => MergeStrategy.concat
  case x => MergeStrategy.first
}