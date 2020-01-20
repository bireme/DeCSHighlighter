name := "DeCSHighlighter"

version := "0.1"

scalaVersion := "2.13.1" // "2.12.9"

val circeVersion = "0.13.0-M2" //"0.12.0-M4" //"0.11.1" //"0.10.0"
val scalajVersion = "2.4.2" //"2.4.1"
val servletApiVersion = "4.0.1" //"3.0.1"
//val hairyfotrVersion = "0.1.17"
val scalaTestVersion = "3.2.0-M2" //"3.1.0-SNAP13" //"3.0.8" //"3.0.7"
val supersafeVersion = "1.1.7"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "org.scalaj" %% "scalaj-http" % scalajVersion,
  "javax.servlet" % "javax.servlet-api" % servletApiVersion % "provided",
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
  //"com.artima.supersafe" %% "supersafe" % supersafeVersion
)

logBuffered in Test := false
trapExit := false

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")
//addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % hairyfotrVersion)

enablePlugins(JettyPlugin)

javaOptions in Jetty ++= Seq(
  "-Xdebug",
  "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8000"
)

containerPort := 7171
