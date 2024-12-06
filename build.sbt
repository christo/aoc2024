import sbt.addCommandAlias

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2024",
    addCommandAlias("day01", "runMain com.chromosundrift.aoc2024.Day01"),
    addCommandAlias("day02", "runMain com.chromosundrift.aoc2024.Day02"),
    addCommandAlias("day03", "runMain com.chromosundrift.aoc2024.Day03"),
    addCommandAlias("day04", "runMain com.chromosundrift.aoc2024.Day04"),

    commands += Command.command("listDays") { state =>
      Project.extract(state).runTask(Compile / discoveredMainClasses, state) match {
        case (newState, mainClasses) =>
          println("Available days:")
          mainClasses.foreach(main => println(s"  $main"))
          newState
      }
    }
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test