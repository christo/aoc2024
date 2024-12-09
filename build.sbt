import sbt._
import Keys._

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.4"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2024",
    addCommandAlias("day01", "runMain com.chromosundrift.aoc2024.Day01"),
    addCommandAlias("day02", "runMain com.chromosundrift.aoc2024.Day02"),
    addCommandAlias("day03", "runMain com.chromosundrift.aoc2024.Day03"),
    addCommandAlias("day04", "runMain com.chromosundrift.aoc2024.Day04"),
    addCommandAlias("day05", "runMain com.chromosundrift.aoc2024.Day05"),
    addCommandAlias("day06", "runMain com.chromosundrift.aoc2024.Day06"),
    addCommandAlias("day07", "runMain com.chromosundrift.aoc2024.Day07"),
    addCommandAlias("day08", "runMain com.chromosundrift.aoc2024.Day08"),
    addCommandAlias("day09", "runMain com.chromosundrift.aoc2024.Day09"),

    commands += Command.command("listDays") { state =>
      Project.extract(state).runTask(Compile / discoveredMainClasses, state) match {
        case (newState, mainClasses) =>
          println("Available days:")
          mainClasses.foreach(main => println(s"  $main"))
          newState
      }
    },

    commands += Command.command("runAll") { state =>
      Project.extract(state).runTask(Compile / discoveredMainClasses, state) match {
        case (newState, mainClasses) =>
          val dayClasses = mainClasses.filter(_.contains("Day")).sorted
          var currentState = newState
          println("\nRunning all days:")

          dayClasses.foreach { mainClass =>
            val start = System.nanoTime()
            println(s"\n=== Running $mainClass ===")

            val cmd = s"runMain $mainClass"
            currentState = Command.process(cmd, currentState, s =>
              println(s"Error running $mainClass: $s"))
            println(s"=== Completed in ${(System.nanoTime() - start) / 1000000}ms ===")
          }

          currentState
      }
    }
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test