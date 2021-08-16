ThisBuild / scalaVersion := "3.0.1"
ThisBuild / organization := "icu.harx"
ThisBuild / organizationName := "harx"

lazy val root = project.in(file("."))
  .settings(
    name := "brainfuck.scala",
    version := "0.1",
    idePackagePrefix := Some("icu.harx")
  )