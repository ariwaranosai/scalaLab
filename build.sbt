name := "scalaLab"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.0",
    "org.scalatest" %% "scalatest" % "2.2.0" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "com.jsuereth" %% "scala-arm" % "1.4",
    "com.chuusai" %% "shapeless" % "2.3.0",
    "org.scala-lang" % "scala-reflect" % "2.11.7",
    "org.scalaz" %% "scalaz-effect" % "7.2.0"
)