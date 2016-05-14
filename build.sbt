import ReleaseTransformations._

lazy val commonSettings = Seq(
  organization := "com.rklaehn",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.10.6", "2.11.8"),
  libraryDependencies ++= Seq(
    "io.circe" %% "circe-core" % "0.4.0",
    "com.typesafe.akka" %% "akka-http-core" % "2.4.4",
    "com.typesafe.akka" %% "akka-http-experimental" % "2.4.4",
    "de.heikoseeberger" %% "akka-http-circe" % "1.6.0",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.6" % "test"
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature"
  ),
  licenses += ("Apache License, Version 2.0", url("http://www.apache.org/licenses/LICENSE-2.0.txt")),
  homepage := Some(url("http://github.com/rklaehn/ipfs-client")),

  // release stuff
  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  releaseCrossBuild := true,
  releasePublishArtifactsAction := PgpKeys.publishSigned.value,
  publishMavenStyle := true,
  publishArtifact in Test := false,
  pomIncludeRepository := Function.const(false),
  initialCommands in console := """
    |import com.rklaehn.ipfs.client._
    |import com.rklaehn.ipfs._
    |implicit val system = akka.actor.ActorSystem("test")
    |import system.dispatcher
    |val ipfs = IPFS("/ip4/127.0.0.1/tcp/5001")
    |""".stripMargin,
  publishTo <<= version { v =>
    val nexus = "https://oss.sonatype.org/"
    if (v.trim.endsWith("SNAPSHOT"))
      Some("Snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("Releases" at nexus + "service/local/staging/deploy/maven2")
  },
  pomExtra :=
    <scm>
      <url>git@github.com:rklaehn/ipfs-client.git</url>
      <connection>scm:git:git@github.com:rklaehn/ipfs-client.git</connection>
    </scm>
    <developers>
      <developer>
        <id>r_k</id>
        <name>R&#xFC;diger Klaehn</name>
        <url>http://github.com/rklaehn/</url>
      </developer>
    </developers>
  ,
  releaseProcess := Seq[ReleaseStep](
    checkSnapshotDependencies,
    inquireVersions,
    runClean,
    ReleaseStep(action = Command.process("package", _)),
    setReleaseVersion,
    commitReleaseVersion,
    tagRelease,
    ReleaseStep(action = Command.process("publishSigned", _)),
    setNextVersion,
    commitNextVersion,
    ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
    pushChanges))

lazy val noPublish = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false)

lazy val root = project.in(file("."))
  .aggregate(core)
  .settings(name := "root")
  .settings(commonSettings: _*)
  .settings(noPublish: _*)

lazy val core = project.in(file("."))
  .settings(name := "ipfs-client")
  .settings(commonSettings: _*)
