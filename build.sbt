lazy val root = project
  .in(file("."))
  .settings(
    name := "Simple-Interpreter",
    description := "A Simple Interpreter For Pascal",
    version := "0.1.0", // 项目版本
    scalaVersion := "3.6.4", // scala版本
    scalacOptions ++= Seq("-deprecation"), // scala编译器选项
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.0" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

  )
