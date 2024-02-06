ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "Tetrus",
    libraryDependencies += "org.scalafx" %% "scalafx" % "21.0.0-R32"
  )


