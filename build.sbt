// voir http://www.wartremover.org/
lazy val warts = {
  import Wart._
  Warts.allBut(FinalVal, StringPlusAny, ToString, Any, Nothing)
}
  

lazy val globalSettings: Seq[sbt.Def.SettingsDefinition] =
  Seq(
    inThisBuild(
      List(
        organization := "chrilves",
        scalaVersion := "2.13.1",
        version := "0.1.0-SNAPSHOT"
      )),
    scalacOptions += "-deprecation",
    updateOptions := updateOptions.value.withCachedResolution(true),
    wartremoverErrors in (Compile, compile) := warts,
    wartremoverWarnings in (Compile, console) := warts,
    addCompilerPlugin("io.tryp" % "splain" % "0.5.1" cross CrossVersion.patch),
    addCompilerPlugin("org.typelevel" % "kind-projector" % "0.10.3" cross CrossVersion.binary),
    scalafmtOnCompile := true,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.1.1" % Test
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
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0",
      scalaJSUseMainModuleInitializer := false
    )
    .dependsOn(coreJS)
