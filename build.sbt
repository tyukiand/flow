lazy val root = (project in file(".")).
settings(
  inThisBuild(List(
    organization := "org.forest-ml",
    scalaVersion := "2.12.6",
    version      := "0.1"
  )),
  name := "flow",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test,
  fork in run := true
)

