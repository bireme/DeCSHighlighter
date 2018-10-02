name := "DeCSHighlighter"

version := "0.1"

scalaVersion := "2.12.7"

val vertxVersion = "3.5.3"
val circeVersion = "0.9.3"
val scalajVersion = "2.4.1"
val servletVersion = "2.5"
val xsbtVersion = "3.0.1"
val hairyfotrVersion = "0.1.17"
val scalaTestVersion = "3.0.5"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % scalajVersion,
  "javax.servlet" % "servlet-api" % servletVersion % "provided",
  "javax.servlet" % "javax.servlet-api" % xsbtVersion % "provided",
  "org.scalatest" % "scalatest_2.12" % scalaTestVersion % "test"
)

logBuffered in Test := false
trapExit := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % hairyfotrVersion)

enablePlugins(JettyPlugin)

javaOptions in Jetty ++= Seq(
  "-Xdebug",
  "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8000"
)

containerPort := 7171
