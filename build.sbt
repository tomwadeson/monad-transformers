lazy val root = (project in file(".")).
  settings(
    organization := "com.tomwadeson",
    version := "0.1.0",
    scalaVersion := "2.12.1",
    name := "monad-transformers"
  )

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.8"
