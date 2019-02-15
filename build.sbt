import Dependencies._

name := "santa"
version := "1.0"
organization := "moon"

//scalaVersion := "2.12.6"
//libraryDependencies ++= {
//  val akkaVersion = "2.4.14"
//  Seq(
//    "com.typesafe.akka"       %% "akka-actor"                        % akkaVersion,
//    "com.typesafe.akka"       %% "akka-slf4j"                        % akkaVersion,
//    "com.typesafe.akka"       %% "akka-remote"                       % akkaVersion,
//    "com.typesafe.akka"       %% "akka-cluster"                      % akkaVersion,
//    "com.typesafe.akka"       %% "akka-multi-node-testkit"           % akkaVersion   % "test",
//    "com.typesafe.akka"       %% "akka-testkit"                      % akkaVersion   % "test",
//    "org.scalatest"           %% "scalatest"                         % "3.0.1"       % "test" exclude("org.scala-lang.modules", "scala-xml_2.11"),
//    "com.typesafe.akka"       %% "akka-slf4j"                        % akkaVersion,
//    "ch.qos.logback"          %  "logback-classic"                   % "1.1.7"
//  )
//}

libraryDependencies  ++= Seq(
  "org.vegas-viz" %% "vegas" % "0.3.11"
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

resolvers ++= Seq(
  // other resolvers here
  // if you want to use snapshot builds (currently 0.12-SNAPSHOT), use this.
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "MapR Repository" at "http://repository.mapr.com/maven/",
  "Vegas Repository" at "https://mvnrepository.com/artifact/org.vegas-viz/vegas"
)

resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases"

logBuffered in Test := false

// or 2.11.8
scalaVersion := "2.11.8"