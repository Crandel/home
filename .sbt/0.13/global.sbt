import org.ensime.EnsimeKeys._

ensimeServerVersion in ThisBuild := "2.0.0-SNAPSHOT" // or "1.0.1"

resolvers += "JBoss" at "https://repository.jboss.org/"

scalacOptions += "-Xlog-implicits"

libraryDependencies += "org.scala-debugger" %% "scala-debugger-api" % "1.1.0-M3"
