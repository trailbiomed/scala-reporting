import org.scalajs.linker.interface.ModuleKind
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / scalaVersion    := "3.3.7"
ThisBuild / organization    := "io.github.pityka"
ThisBuild / homepage        := Some(url("https://github.com/trailbiomed/scala-reporting"))
ThisBuild / licenses        := Seq("MIT" -> url("https://opensource.org/licenses/MIT"))
ThisBuild / scmInfo         := Some(
  ScmInfo(
    url("https://github.com/trailbiomed/scala-reporting"),
    "scm:git@github.com:trailbiomed/scala-reporting.git"
  )
)
ThisBuild / developers      := List(
  Developer("trailbiomed", "Trail Biomed", "ibartha@trailbiomed.com", url("https://github.com/trailbiomed"))
)

ThisBuild / dynverSeparator := "-"

ThisBuild / Compile / doc / sources := Seq.empty

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Wvalue-discard",
  "-Xfatal-warnings",
  "-no-indent",
  "-encoding",
  "utf-8"
)

val ghOwner    = "trailbiomed"
val ghRepoName = "scala-reporting"


ThisBuild / resolvers += "GitHub Package Registry (scala-lui)" at
  s"https://maven.pkg.github.com/$ghOwner/scala-lui"

ThisBuild / credentials += Credentials(
  "GitHub Package Registry",
  "maven.pkg.github.com",
  sys.env.getOrElse("GITHUB_ACTOR", "x-access-token"),
  sys.env.getOrElse("GITHUB_TOKEN", "")
)

val jsoniterVersion = "2.30.4"
val laminarVersion  = "17.1.0"
val nsplVersion     = "0.18.0"
val saddleVersion   = "4.0.0-M14"
val luiVersion      = "0.6.0"

lazy val root = (project in file("."))
  .aggregate(sharedJS, sharedJVM, browser, jvm, example)
  .settings(publish / skip := true, name := "trail-reporting")

lazy val shared = (crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure) in file("shared"))
  .settings(
    name := "trail-reporting-shared",
    libraryDependencies ++= Seq(
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-core"   % jsoniterVersion,
      "com.github.plokhotnyuk.jsoniter-scala" %%% "jsoniter-scala-macros" % jsoniterVersion % "compile-internal"
    )
  )

lazy val sharedJS  = shared.js
lazy val sharedJVM = shared.jvm

lazy val browser = (project in file("browser"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(sharedJS)
  .settings(
    name := "trail-reporting-browser",
    libraryDependencies ++= Seq(
      "io.github.pityka" %%% "lui-core"       % luiVersion,
      "io.github.pityka" %%% "lui-components" % luiVersion
    ),
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= (
      _.withModuleKind(ModuleKind.NoModule)
        .withSourceMap(false)
    ),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory :=
      (Compile / target).value / "browser-fast",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory :=
      (Compile / target).value / "browser-full",
  )

lazy val jvm = (project in file("jvm"))
  .dependsOn(sharedJVM)
  .settings(
    name := "trail-reporting",
    libraryDependencies ++= Seq(
      "io.github.pityka" %% "nspl-awt"    % nsplVersion,
      "io.github.pityka" %% "saddle-core" % saddleVersion
    ),
    Compile / resourceGenerators += Def.task {
      val _      = (browser / Compile / fullLinkJS).value
      val outDir = (browser / Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
      val target = (Compile / resourceManaged).value / "trail-reporting"
      IO.createDirectory(target)
      val produced = (outDir ** "*.js").get
      val copied = produced.map { f =>
        val dst = target / "browser.js"
        IO.copyFile(f, dst)
        dst
      }
      copied
    }.taskValue
  )

lazy val example = (project in file("example"))
  .dependsOn(jvm)
  .settings(
    name := "trail-reporting-example",
    publish / skip := true,
    fork := true,
    Compile / mainClass := Some("trail.reporting.example.Main"),
    Compile / run / baseDirectory := (LocalRootProject / baseDirectory).value
  )
