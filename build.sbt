Global / onChangedBuildSource := ReloadOnSourceChanges

lazy val scala213 = "2.13.6"

inThisBuild(
  scalaVersion := scala213
)

lazy val `scalajs-laminar-spreadsheet` = project
  .in(file("."))
  .enablePlugins(ScalaJSPlugin)
  .enablePlugins(ScalaJSBundlerPlugin)
  .settings(
    normalizedName := "scalajs-laminar-spreadsheet",
    Compile / fastOptJS / scalaJSLinkerConfig ~= { _.withSourceMap(true) },
    Compile / fullOptJS / scalaJSLinkerConfig ~= { _.withSourceMap(false) },
    publish / skip := true,
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "0.13.1",
      "com.lihaoyi" %%% "fastparse" % "2.3.3"
    )
  )
