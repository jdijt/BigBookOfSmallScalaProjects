lazy val root = project
  .in(file("."))
  .settings(
    name         := "BigBookOfSmallScalaProjects",
    version      := "0.1.0",
    scalaVersion := "3.2.1",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % "3.4.4",
      "org.scalameta" %% "munit"       % "1.0.0-M7" % Test
    )
  )
