resolvers += Resolver.sonatypeRepo("releases")

lazy val commonSettings = Seq(
  version := "0.1.0",
  resolvers += "scalatl" at "http://milessabin.com/scalatl",
  scalaVersion := "2.11.8",
  addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.0.1" cross CrossVersion.full) //without plugin compile will fail
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "scalaLab",
    scalacOptions ++= Seq(
      "-feature",
      "-language:higherKinds"
    ),

    libraryDependencies ++= Seq(
    "org.scalaz" %% "scalaz-core" % "7.2.0",
    "org.scalatest" %% "scalatest" % "2.2.0" % "test",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "com.jsuereth" %% "scala-arm" % "1.4",
    "com.chuusai" %% "shapeless" % "2.3.0",
    "org.scala-lang" % "scala-reflect" % "2.11.7",
    "org.scalaz" %% "scalaz-effect" % "7.2.0",
    "com.milessabin" % "si2712fix-library" % "1.0.1" cross CrossVersion.full
   )
  )

