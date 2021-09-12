name := "fuzzy"

val apiVersion            = "0.3.0"
val apiTestkitVersion     = "0.2.0"
val cliVersion            = "0.3.0"
val matcherTestkitVersion = "0.3.0"
val matcherSimpleVersion  = "0.2.1"
val matcherLoopVersion    = "0.2.0"

// TODO upgrade to latest stable version of libraries: last updated in 2020, but newer vesion of cats-mtl probably bring in newer versions then stated below
val catsCore      = "org.typelevel" %% "cats-core"      % "2.1.1"
val catsEffect    = "org.typelevel" %% "cats-effect"    % "2.1.4"
val declineEffect = "com.monovore"  %% "decline-effect" % "1.0.0"
val fastparse     = "com.lihaoyi"   %% "fastparse"      % "2.3.2"
val hedgehogSbt   = "qa.hedgehog"   %% "hedgehog-sbt"   % "4d4763691024de171c6e10f6bd9aa996a174d296"

def testDependencies(dependencyConfig: String) = List(hedgehogSbt).map(_ % dependencyConfig)

val apiDeps            = List(fastparse)
val apiTestkitDeps     = testDependencies("compile")
val cliDeps            = List(catsCore, catsEffect, declineEffect)
val matcherSimpleDeps  = List()
val matcherTestkitDeps = testDependencies("compile")

lazy val api = (project in file("api"))
  .settings(
    name := "fuzzy-api",
    version := apiVersion,
    testSettings(),
    libraryDependencies ++= apiDeps
  )

lazy val apiTestkit = (project in file("api-testkit"))
  .settings(
    name := "fuzzy-api-testkit",
    version := apiTestkitVersion,
    libraryDependencies ++= apiTestkitDeps
  )
  .dependsOn(api)

lazy val apiTests = (project in file("api-tests"))
  .settings(
    name := "fuzzy-api-tests",
    version := apiVersion,
    testSettings()
  )
  .dependsOn(api % "test", apiTestkit % "test")

lazy val matcherTestkit = (project in file("matcher-testkit"))
  .settings(
    name := "fuzzy-matcher-testkit",
    version := matcherTestkitVersion,
    libraryDependencies ++= matcherTestkitDeps
  )
  .dependsOn(api, apiTestkit)

lazy val matcherSimple = (project in file("matcher-simple"))
  .settings(
    name := "fuzzy-matcher-simple",
    version := matcherSimpleVersion,
    libraryDependencies ++= matcherSimpleDeps,
    scalacOptions += "-Ypartial-unification", // TODO remove once we upgrade scala
    testSettings()
  )
  .dependsOn(api, apiTestkit % "test", matcherTestkit % "test")

lazy val matcherLoop = (project in file("matcher-loop"))
  .settings(
    name := "fuzzy-matcher-loop",
    version := matcherLoopVersion,
    testSettings()
  )
  .dependsOn(api, apiTestkit % "test", matcherTestkit % "test", matcherSimple % "test")

lazy val cli = (project in file("cli"))
  .settings(
    name := "fuzzy",
    version := cliVersion,
    libraryDependencies ++= cliDeps
  )
  .dependsOn(api, matcherLoop)

def testSettings() = Seq(
  resolvers += "bintray-scala-hedgehog" at "https://dl.bintray.com/hedgehogqa/scala-hedgehog",
  testFrameworks += TestFramework("hedgehog.sbt.Framework"),
  libraryDependencies ++= testDependencies("test")
)

