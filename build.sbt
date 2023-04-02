name := "bluez-experiments"
scalaVersion := "3.2.2"
scalacOptions ++= Seq(
  "-Ykind-projector",
  // "-Yexplicit-nulls",
  "-explain",
  "-source:future-migration",
  "-rewrite"
)

libraryDependencies ++= Seq(
  "com.github.hypfvieh" % "bluez-dbus" % "0.1.4",
  "org.typelevel" %% "cats-effect" % "3.4.8",
  "co.fs2" %% "fs2-io" % "3.6.1",
  "co.fs2" %% "fs2-scodec" % "3.6.1",
  "com.outr" %% "scribe-slf4j" % "3.11.1",
  "com.outr" %% "scribe-cats" % "3.11.1",
  "org.scala-lang.modules" %% "scala-xml" % "2.1.0"
)