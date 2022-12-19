name := "DeCSHighlighter"

version := "0.1"

scalaVersion := "2.13.10"

val playJsonVersion = "2.9.3" //"2.9.1"
val scalajVersion = "2.4.2" //"2.4.1"
//val servletApiVersion = "4.0.1" //"3.0.1"
val jakartaServletApiVersion = "6.0.0"
val scalaTestVersion = "3.3.0-SNAP3" //"3.2.0-M2"
val supersafeVersion = "1.1.7"
val luceneVersion = "9.4.2" //"8.7.0"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % playJsonVersion,
  "org.scalaj" %% "scalaj-http" % scalajVersion,
  //"javax.servlet" % "javax.servlet-api" % servletApiVersion % "provided",
  "jakarta.servlet" % "jakarta.servlet-api" % jakartaServletApiVersion % "provided",
  "org.apache.lucene" % "lucene-core" % luceneVersion,
  "org.apache.lucene" % "lucene-analysis-common" % luceneVersion,
  "org.apache.lucene" % "lucene-backward-codecs" % luceneVersion,
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test"
)

Test / logBuffered := false
trapExit := false

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")

enablePlugins(JettyPlugin)

Jetty / javaOptions ++= Seq(
  "-Xdebug",
  "-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8000"
)

containerPort := 7171

/*
assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}*/

assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}


//test in assembly := {}
