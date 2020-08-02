name := "fuzzy"
version := "0.2.0"

resolvers += "bintray-scala-hedgehog" at "https://dl.bintray.com/hedgehogqa/scala-hedgehog"

testFrameworks += TestFramework("hedgehog.sbt.Framework")

libraryDependencies ++= List(
  "com.lihaoyi"   %% "fastparse"    % "2.2.2",
  "qa.hedgehog"   %% "hedgehog-sbt" % "4d4763691024de171c6e10f6bd9aa996a174d296" % "test"
)
