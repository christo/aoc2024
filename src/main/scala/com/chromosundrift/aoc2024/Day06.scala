package com.chromosundrift.aoc2024

import scala.io.Source

case class Guard(dir: Char, loc: (Int, Int)) {
  // (row,col)
  override def toString: String = s"$dir at position $loc"
}

case class GuardMap(rows: Array[String], guard: Guard, steps: Int, obstacle: Option[(Int, Int)]) {
  override def toString: String = {
    val mapStr = rows.mkString("\n")
    s"""Map:
       |$mapStr
       |Guard: $guard
       |Obstacle: $obstacle
       |Steps: $steps""".stripMargin
  }
}

//noinspection DuplicatedCode
object Day06 {
  def getInput(file: String): String = Source.fromResource(file).mkString

  private val guardRegex = """[>v<^]""".r
  private val directions = ">v<^" // clockwise so right turn is index + 1

  private def delta(c: Char): (Int, Int) = {
    c match
      case '>' => (0, 1)
      case 'v' => (1, 0)
      case '<' => (0, -1)
      case '^' => (-1, 0)
      case _ => throw new IllegalStateException(s"unknown direction ${c}")
  }

  def main(args: Array[String]): Unit = {
    val sample1 = parse(getInput("day06_sample1.txt"))
    val input1 = parse(getInput("day06_input1.txt"))
    println(part1(sample1))
    println(part1(input1))
    println(part2(sample1))
    println(part2(input1))
  }


  private def turnRight(c: Char): Char = directions.charAt((directions.indexOf(c) + 1) % 4)

  def parse(str: String): GuardMap = {
    val rows = str.split('\n')
    val g = rows.zipWithIndex.flatMap { case (line, rowIndex) =>
      guardRegex.findFirstMatchIn(line).map { matchResult =>
        Guard(matchResult.group(0).head, (rowIndex, matchResult.start))
      }
    }.headOption.getOrElse(throw new NoSuchElementException("no guard"))
    GuardMap(rows, g, 0, None)
  }

  def inBounds(loc: (Int, Int), gm: GuardMap): Boolean = {
    val r = loc._1
    val c = loc._2
    !(r < 0 || c < 0 || r >= gm.rows.length || c >= gm.rows(0).length)
  }

  def blocked(loc: (Int, Int), gm: GuardMap): Boolean = {
    gm.rows(loc._1).charAt(loc._2).equals('#') || gm.obstacle.contains(loc)
  }

  def step(gm: GuardMap, visited: scala.collection.mutable.Set[(Int, Int, Char)]): Option[GuardMap] = {
    val g = gm.guard
    val d = delta(g.dir)
    val newLoc = (g.loc._1 + d._1, g.loc._2 + d._2)

    // Check if we've seen this state before
    val state = (g.loc._1, g.loc._2, g.dir)
    if (visited.contains(state)) {
      None  // We've been here in this direction before - it's a cycle
    } else {
      visited.add(state)  // Add current state to visited set
      newLoc match
        case (r, c) if inBounds((r, c), gm) && !blocked((r, c), gm) =>
          Some(GuardMap(gm.rows, Guard(g.dir, (r, c)), gm.steps + 1, gm.obstacle))
        case (r, c) if inBounds((r, c), gm) && blocked((r, c), gm) =>
          Some(GuardMap(gm.rows, Guard(turnRight(g.dir), g.loc), gm.steps + 1, gm.obstacle))
        case _ => None // out of bounds
    }
  }

  def collect(x: GuardMap): List[GuardMap] = {
    val visited = scala.collection.mutable.Set[(Int, Int, Char)]()

    @scala.annotation.tailrec
    def loop(current: GuardMap, acc: List[GuardMap]): List[GuardMap] = {
      step(current, visited) match {
        case Some(next) => loop(next, acc.appended(current))
        case None => acc.appended(current)
      }
    }

    loop(x, List.empty)
  }

  def part1(gm: GuardMap): Int = collect(gm).map(_.guard.loc).toSet.size

  def guardLoops(gm: GuardMap): Boolean = {
    val visited = scala.collection.mutable.Set[(Int, Int, Char)]()
    var current = gm

    while (true) {
      val state = (current.guard.loc._1, current.guard.loc._2, current.guard.dir)
      if (visited.contains(state)) {
        return true  // Found a cycle
      }
      visited.add(state)

      step(current, visited) match {
        case Some(next) => current = next
        case None => return visited.contains((current.guard.loc._1, current.guard.loc._2, current.guard.dir))
      }
    }
    false  // This line won't be reached but Scala needs it
  }

  def part2(gm: GuardMap): Int = {
    val locations = for {
      r <- gm.rows.indices
      c <- gm.rows(0).indices
      if gm.rows(r)(c) == '.' && guardLoops(GuardMap(gm.rows, gm.guard, 0, Some(r, c)))
    } yield (r, c)
    locations.length
  }
}