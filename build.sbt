// SPDX-License-Identifier: Apache-2.0

def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

//git.remoteRepo := "git@github.com:milovanovic/awg.git"
name := "awg"

val commonSettings = Seq(
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.12.12",
  crossScalaVersions := Seq("2.12.12", "2.11.12"),
  scalacOptions ++= scalacOptionsVersion(scalaVersion.value),
  javacOptions ++= javacOptionsVersion(scalaVersion.value),
  resolvers ++= Seq (
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),
  libraryDependencies ++= Seq(
    "org.scalanlp" %% "breeze-viz" % "0.13.2",
    "edu.berkeley.cs" %% "rocket-dsptools" % "1.2-SNAPSHOT"
  )
)

lazy val plfg = (project in file("generators/plwg"))
  .settings(commonSettings: _*)

lazy val nco = (project in file("generators/nco"))
  .settings(commonSettings: _*)

lazy val fft = (project in file("generators/sdf-fft"))
  .settings(commonSettings: _*)


lazy val singleNCOwithMultiplyingChain = (project in file("."))
  .dependsOn(plfg, nco, fft)
  .settings(commonSettings: _*)
