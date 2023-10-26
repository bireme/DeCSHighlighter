name := "DeCSHighlighter"

version := "0.1"

scalaVersion := "2.13.12"

val playJsonVersion = "2.10.2" //"2.9.4"
val scalajVersion = "2.4.2" //"2.4.1"
val jakartaServletApiVersion = "6.0.0"
val scalaTestVersion = "3.3.0-SNAP4" //"3.2.0-M2"
val luceneVersion = "9.8.0" //"9.7.0"
val logback = "1.4.11" //"1.4.8"
val logging = "3.9.5"

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % playJsonVersion,
  "org.scalaj" %% "scalaj-http" % scalajVersion,
  "jakarta.servlet" % "jakarta.servlet-api" % jakartaServletApiVersion % "provided",
  "org.apache.lucene" % "lucene-core" % luceneVersion,
  "org.apache.lucene" % "lucene-analysis-common" % luceneVersion,
  "org.apache.lucene" % "lucene-backward-codecs" % luceneVersion,
  "org.apache.lucene" % "lucene-codecs" % luceneVersion,
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "ch.qos.logback" % "logback-classic" % logback,
  "com.typesafe.scala-logging" %% "scala-logging" % logging
)

Test / logBuffered := false
trapExit := false

resolvers += "Artima Maven Repository" at "https://repo.artima.com/releases"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Ywarn-unused")

//enablePlugins(JettyPlugin)
enablePlugins(TomcatPlugin)

assembly / assemblyMergeStrategy := {
  case "module-info.class" => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy).value
    oldStrategy(x)
}

/*
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", _*) => MergeStrategy.discard
  case _                        => MergeStrategy.first
}
*/

//test in assembly := {}
