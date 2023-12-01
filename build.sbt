lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      scalaVersion := "2.13.12"
    )
  ),
  name := "advent-of-code"
)

libraryDependencies ++= Seq(
  // Cats - Functional programming library
  "org.typelevel" %% "cats-core" % "2.6.1",

  // Cats Effect - Effect type and concurrency library
  "org.typelevel" %% "cats-effect" % "3.2.8",

  // Fs2 - Functional Streams for Scala
  "co.fs2" %% "fs2-core" % "3.1.6",
  "co.fs2" %% "fs2-io" % "3.9.3",

  // Scala Test
  "org.scalatest" %% "scalatest" % "3.2.17" % Test,

  // Cats Effect Testing - Library for testing Cats Effect programs
  "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test
)
