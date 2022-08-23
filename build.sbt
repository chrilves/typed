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
        scalaVersion := "3.1.3",
        version := "0.1.0-SNAPSHOT"
      )),
    scalacOptions += "-deprecation",
    updateOptions := updateOptions.value.withCachedResolution(true),
    Compile/compile/wartremoverErrors := warts,
    Compile/console/wartremoverErrors := warts,
    scalafmtOnCompile := true,
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.13" % Test
  )

lazy val core =
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure)
    .in(file("core"))
    .settings(globalSettings : _*)
    .settings(name := "typed-core")
    .jsSettings(scalacOptions += "-scalajs")

lazy val coreJS = core.js
lazy val coreJVM = core.jvm

lazy val web =
  project
    .in(file("web"))
    .enablePlugins(ScalaJSPlugin)
    .settings(globalSettings : _*)
    .settings(
      name := "typed-web",
      scalacOptions += "-scalajs",
      libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.2.0",
      scalaJSUseMainModuleInitializer := false,
    )
    .dependsOn(coreJS)
