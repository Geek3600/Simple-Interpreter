lazy val root = project
  .in(file("."))
  .settings(
    name := "Simple-Interpreter",
    description := "A Simple Interpreter For Pascal",
    version := "0.1.0", // 项目版本
    scalaVersion := "3.6.4", // scala版本
    scalacOptions ++= Seq("-deprecation"), // scala编译器选项
    libraryDependencies += "org.scalameta" %% "munit" % "1.1.0" % Test,
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.5",
    libraryDependencies += "org.typelevel" %% "case-insensitive" % "1.4.2",
    libraryDependencies += "org.bytedeco" % "llvm" % "16.0.4-1.5.9",
    libraryDependencies += "org.bytedeco" % "llvm" % "16.0.4-1.5.9" classifier "linux-x86_64"
  )
