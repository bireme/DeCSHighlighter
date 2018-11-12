name := "DeCSHighlighter"

version := "0.1"

scalaVersion := "2.12.7"

val circeVersion = "0.10.0" // "0.9.3"
val scalajVersion = "2.4.1"
val servletVersion = "2.5"
val servletApiVersion = "4.0.1" //"3.0.1"
val hairyfotrVersion = "0.1.17"
val scalaTestVersion = "3.0.5"
val supersafeVersion = "1.1.7"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % scalajVersion,
  "javax.servlet" % "servlet-api" % servletVersion % "provided",
  "javax.servlet" % "javax.servlet-api" % servletApiVersion % "provided",
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" % "scalatest_2.12" % scalaTestVersion % "test"
  //"com.artima.supersafe" %% "supersafe" % supersafeVersion
)

logBuffered in Test := false
trapExit := false

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")
addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % hairyfotrVersion)

enablePlugins(JettyPlugin)

javaOptions in Jetty ++= Seq(
  "-Xdebug",
  "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8000"
)

containerPort := 7171
