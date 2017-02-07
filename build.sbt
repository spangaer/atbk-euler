lazy val root = (project in file("."))
  .settings(
    name         := "atbk-euler",
    organization := "eu.atbk.euler",
    scalaVersion := "2.11.8",
    version      := "0.1.0-SNAPSHOT"
  ).settings(
    EclipseKeys.eclipseOutput := Some("target"),
    EclipseKeys.executionEnvironment := Some(EclipseExecutionEnvironment.JavaSE18),
    EclipseKeys.withSource := true,
    EclipseKeys.withJavadoc := true,
    //EclipseKeys.withBundledScalaContainers := false,
    crossPaths := false
  ).settings(
    scalacOptions ++= Seq("-Ybackend:GenBCode", "-Ydelambdafy:method", "-target:jvm-1.8","-Xexperimental" ,"-unchecked", "-deprecation", "-encoding", "utf8","-feature")
  ).settings(
  	libraryDependencies += "com.ibm.icu" % "icu4j" % "58.2",
  	libraryDependencies += "com.google.guava" % "guava" % "21.0"
  )
