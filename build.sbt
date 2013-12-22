import AssemblyKeys._ 
assemblySettings

/** Project */
name := "triplerush"

version := "1.0-SNAPSHOT"

organization := "com.signalcollect"

scalaVersion := "2.10.3"

scalacOptions ++= Seq("-optimize", "-Yinline-warnings", "-feature", "-deprecation")

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.withSource := true

test in assembly := {}

parallelExecution in Test := false

excludedJars in assembly <<= (fullClasspath in assembly) map { cp => 
  cp filter {_.data.getName == "minlog-1.2.jar"}
}

/** Dependencies */
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.10.3"  % "compile",
  "com.google.collections" % "google-collections" % "1.0",
  "junit" % "junit" % "4.8.2"  % "test",
  "org.specs2" %% "specs2" % "2.3.3"  % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.0" % "test",
  "org.scalatest" %% "scalatest" % "2.0.1-SNAP" % "test",
  "org.easymock" % "easymock" % "3.2" % "test",
  "org.apache.jena" % "apache-jena-libs" % "2.11.0" % "test",
  "org.openrdf.sesame" % "sesame-runtime" % "2.7.8" % "test"
  )
