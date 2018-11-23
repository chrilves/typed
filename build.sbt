// shadow sbt-scalajs' crossProject and CrossType until Scala.js 1.0.0 is released
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

// voir http://www.wartremover.org/
lazy val warts =
  Warts.allBut(Wart.FinalVal)

lazy val globalSettings: Seq[sbt.Def.SettingsDefinition] =
  Seq(
    inThisBuild(
      List(
        organization := "chrilves",
        scalaVersion := "2.12.8",
        version := "0.1.0-SNAPSHOT"
      )),
    updateOptions := updateOptions.value.withCachedResolution(true),
    wartremoverErrors in (Compile, compile) := warts,
    wartremoverWarnings in (Compile, console) := warts,
    //addCompilerPlugin("io.tryp" % "splain" % "0.3.4" cross CrossVersion.patch),
    addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.9" cross CrossVersion.binary),
    scalafmtOnCompile := true,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % Test
  )

lazy val core =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(globalSettings : _*)
    .settings(name := "typed-core")

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val web =
  project
    .in(file("web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(globalSettings : _*)
    .settings(
      name := "typed-web",
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6",
      scalaJSUseMainModuleInitializer := false
    )
    .dependsOn(coreJS)