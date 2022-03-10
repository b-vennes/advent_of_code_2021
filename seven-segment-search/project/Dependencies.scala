import sbt._

object Dependencies {
  lazy val test = Seq(
    "org.scalatest" %% "scalatest" % "3.2.9" % Test
  )

  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % "2.7.0",
    "org.typelevel" %% "cats-effect" % "3.3.7",
    "org.typelevel" %% "cats-parse" % "0.3.6"
  )

  lazy val fs2 = Seq(
    "co.fs2" %% "fs2-core" % "3.2.5",
    "co.fs2" %% "fs2-io" % "3.2.5"
  )

  lazy val app: Seq[ModuleID] = cats ++ fs2 ++ test
}
