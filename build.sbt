name := "reverse-template"
version := "0.1.0"

resolvers += "bintray-scala-hedgehog" at "https://dl.bintray.com/hedgehogqa/scala-hedgehog"

testFrameworks += TestFramework("hedgehog.sbt.Framework")

libraryDependencies += "qa.hedgehog" %% "hedgehog-sbt" % "4d4763691024de171c6e10f6bd9aa996a174d296" % "test"
