/**                                                                    **\
**  Copyright (c) 2018-2019 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences                 **
**                Wädenswil, Switzerland                                **
\**                                                                    **/

import scala.language.postfixOps

lazy val scalaVersionJvm   = "2.12.4-bin-typelevel-4"
lazy val scalaVersionJs    = "2.12.4"
lazy val buildOrganization = "cyby"
lazy val buildVersion      = "2.2.0-SNAPSHOT"
lazy val paradiseV         = "2.1.0"


lazy val buildSettings = Seq(
  organization              := buildOrganization,
  version                   := buildVersion,
  addCompilerPlugin("org.scalamacros" % "paradise_2.12.4" % paradiseV),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  libraryDependencies ++= Seq(
    // General functional programming
    "org.typelevel"  %%% "cats-core"      % catsV,

    // JSON encoding and decoding
    "io.circe"       %%% "circe-core"     % circeV,
    "io.circe"       %%% "circe-generic"  % circeV,
    "io.circe"       %%% "circe-shapes"   % circeV,

    // Generic programming
    "com.chuusai"    %%% "shapeless"      % shapelessV,

    //Testing
    "org.typelevel"  %%% "discipline"     % disciplineV % Test,
    "org.scalacheck" %%% "scalacheck"     % scalacheckV % Test,
    "org.scalatest"  %%% "scalatest"      % scalatestV % Test,
    "org.typelevel"  %%% "cats-laws"      % catsV % Test,

    "com.github.alexarchambault" %%% "scalacheck-shapeless_1.13" % "1.1.5" % Test
  )
)

lazy val buildSettingsJvm = Seq(
  scalaVersion              := scalaVersionJvm,
  scalaOrganization         := "org.typelevel",
  scalacOptions             ++= typelevelScalacOptions,
  scalacOptions in (Compile,console) ~= (_ filter consoleScalacOptions.contains),
  scalacOptions in (Test,console) ~= (_ filter consoleScalacOptions.contains),
  scalacOptions in Test     -= "-Xfatal-warnings",
)

lazy val buildSettingsJs = Seq(
  scalaVersion              := scalaVersionJs,
  scalacOptions             ++= commonScalacOptions,
)

lazy val server =
  project.in(file("server"))
    .settings(
      buildSettings ++ buildSettingsJvm,
      name := "cyby-server",
      publishArtifact in Test := true,
      libraryDependencies ++= Seq(
        "org.openscience.cdk" % "cdk-bundle" % "2.0",
        "org.typelevel"  %% "cats-effect"    % catsEffectV,
        // Functional streams
        "co.fs2"         %% "fs2-core"       % fs2V,
        "org.http4s"     %% "http4s-circe"   % http4sV,
        "org.http4s"     %% "http4s-dsl"     % http4sV,
        "org.http4s"     %% "http4s-blaze-server" % http4sV,
        // DB access
        "org.tpolecat"   %% "doobie-core"    % doobieV,
        "org.xerial"     %  "sqlite-jdbc"    % xerialV,
        "io.circe"       %% "circe-jawn"     % circeV,
        "io.circe"       %% "circe-parser"   % circeV,
        // Hashing
        "org.mindrot"   %   "jbcrypt"        % jbcryptV,
      )
    ).dependsOn(utilJvm % testComp)
 
lazy val serverEx =
  project.in(file("example/server"))
    .settings(
     buildSettings ++ buildSettingsJvm,
     name := "cyby-example-server",
     initialCommands in console := serverCommands,
     mainClass in assembly := Some("cyby.server.example.Main"),
     assemblyMergeStrategy in assembly := {
       case PathList(ps @ _*) if ps.contains("MANIFEST.MF") ⇒ MergeStrategy.discard
       case                                             _ ⇒ MergeStrategy.first
     },
     assemblyJarName in assembly := "server.jar",
     libraryDependencies ++= Seq(
       "io.circe"       %% "circe-parser"        % circeV,
       "io.circe"       %% "circe-jawn"          % circeV,
       "co.fs2"         %% "fs2-io"              % fs2V,
       "org.http4s"     %% "http4s-circe"        % http4sV,
       "org.http4s"     %% "http4s-dsl"          % http4sV,
       "ch.qos.logback" %  "logback-classic"     % "1.2.3",
       "org.tpolecat"   %% "doobie-core"         % doobieV,
       "org.xerial"     %  "sqlite-jdbc"         % xerialV,
       "org.mindrot"    %  "jbcrypt"             % jbcryptV,
    )
  ).dependsOn(datJvm % testComp, utilJvm % testComp, server % testComp)

lazy val util =
  crossProject.in(file("util"))
    .settings(
      buildSettings,
      name := "cyby-util",
    )
    .jvmSettings(
      buildSettingsJvm,
      publishArtifact in Test := true,
      initialCommands in console := utilCommands,
      libraryDependencies ++= Seq(
        "org.openscience.cdk" % "cdk-bundle" % "2.0"
      )
    ).jsSettings(buildSettingsJs)

lazy val ui =
  crossProject.in(file("ui"))
    .settings(
      buildSettings ++ buildSettingsJs,
      name := "cyby-ui",
      libraryDependencies ++= Seq(
        "io.circe"     %%% "circe-parser" % circeV,
      ),
    )
    .jvmSettings(buildSettingsJvm)
    .jsSettings(
      buildSettingsJs,
      libraryDependencies ++= Seq(
        "org.scala-js" %%% "scalajs-dom"  % domV,
      ),
    )
    .dependsOn(util % testComp, msf % testComp, js % testComp)

lazy val msf =
  crossProject.in(file("msf"))
    .settings(
      buildSettings,
      name := "cyby-msf",
      libraryDependencies ++= Seq(
        "org.typelevel"  %%% "cats-effect"    % catsEffectV,
      )
    )
    .jvmSettings(buildSettingsJvm)
    .jsSettings(buildSettingsJs)

lazy val js =
  crossProject.in(file("msf_js"))
    .settings(
      buildSettings,
      name := "cyby-msf-js",
      libraryDependencies ++= Seq(
        "org.typelevel"  %%% "cats-effect"    % catsEffectV,
      )
    )
    .jvmSettings(buildSettingsJvm)
    .jsSettings(
      buildSettingsJs,
      libraryDependencies ++= Seq(
        "org.scala-js"   %%% "scalajs-dom"    % domV,
      )
    )
    .dependsOn(msf)

lazy val dat =
  crossProject.in(file("example/dat"))
    .settings(
      buildSettings,
      name := "cyby-example-dat",
    )
    .jvmSettings(
      buildSettingsJvm,
      initialCommands in console := utilCommands,
    )
    .jsSettings(buildSettingsJs)
    .dependsOn(util % testComp)

 lazy val uiEx =
   crossProject.in(file("example/ui"))
     .settings(
       buildSettings,
       name := "cyby-example-ui",
       libraryDependencies ++= Seq(
         "io.circe"     %%% "circe-parser" % circeV,
       ),
     )
     .jvmSettings(
       buildSettingsJvm,
     )
     .jsSettings(
       buildSettingsJs,
       libraryDependencies ++= Seq(
         "org.scala-js" %%% "scalajs-dom"  % domV,
       ),
     )
     .dependsOn(dat % testComp, ui % testComp)

lazy val utilJs   = util.js
lazy val utilJvm  = util.jvm

lazy val jsJS     = js.js
lazy val jsJVM    = js.jvm

lazy val uiJS     = ui.js
lazy val uiJVM    = ui.jvm

lazy val uiExJS   = uiEx.js
lazy val uiExJVM  = uiEx.jvm

lazy val msfJs    = msf.js
lazy val msfJvm   = msf.jvm

lazy val datJs    = dat.js
lazy val datJvm   = dat.jvm

lazy val testComp = "compile;test->test"

lazy val typelevelScalacOptions = Seq(
                                   "-Yinduction-heuristics",
                                   "-Yliteral-types",
                                 ) ++ commonScalacOptions

lazy val commonScalacOptions = Seq(
                                 "-Xcheckinit",
                                 "-Xfuture",
                                 "-Xlint",
//                                 "-Xfatal-warnings",
                                 "-Yno-adapted-args",
                                 "-Ywarn-unused-import",
//                                 "-Xlog-implicits",
                                 "-Ywarn-value-discard"
                               ) ++ consoleScalacOptions

lazy val consoleScalacOptions = Seq(
                                 "-deprecation",
                                 "-feature",
                                 "-language:postfixOps",
                                 "-language:higherKinds",
                                 "-language:implicitConversions",
                                 "-unchecked",
                                 "-Ypartial-unification",
                               )

lazy val utilCommands = """
  import cyby._
  import cyby.query._
  import shapeless.{Id ⇒ _, Default ⇒ _, _}
  import io.circe._
  import cats.implicits._
  """

lazy val serverCommands = utilCommands ++ """
  import cyby.server._
  import cyby.server.zhaw._
  """

val catsEffectV          = "1.3.1"  // taken from http42
val catsV                = "1.6.1"  // taken from http42
val circeV               = "0.11.1"
val disciplineV          = "0.9.0"  // taken from http42
val domV                 = "0.9.4"
val doobieV              = "0.5.3"
val fs2V                 = "1.0.5"  // taken from http42
val http4sV              = "0.20.6"
val jbcryptV             = "0.4"
val scalacheckV          = "1.13.5"
val scalatestV           = "3.0.4"
val shapelessV           = "2.3.3"
val xerialV              = "3.6.16"

// vim: set ts=2 sw=2 filetype=scala nowrap et:
