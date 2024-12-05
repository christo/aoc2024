package com.chromosundrift.aoc2024

import scala.io.Source

type Rules = Array[(Int, Int)]
type PageSequence = Array[Int]
case class Day05Input(rules: Rules, pages: Array[PageSequence])


object Day05 {
  def getInput(file: String): String = {
    Source.fromResource(file).mkString
  }

  def parseInput(s: String): Day05Input = {
    val parts = s.split("\n\n")
    assert(parts.length == 2)
    val rules = parts(0).split("\n").map { l =>
      val ints = l.split('|').map(_.toInt)
      assert(ints.length == 2)
      val rule = (ints(0), ints(1))
      rule
    }
    val pages: Array[Array[Int]] = parts(1).split("\n").map(_.split(",").map(_.toInt))
    new Day05Input(rules, pages)
  }

  def main(args: Array[String]): Unit = {
    val input = parseInput(getInput("day05_input1.txt"))
    println(part1(input))
    println(part2(input))
  }

  def inOrder(ints: PageSequence, rules: Rules): Boolean = {
    val pages: Array[(Int, Int)] = ints.zipWithIndex
    rules.forall { r =>
      val before: Option[(Int, Int)] = pages.find((t: (Int, Int)) => t._1 == r._1)
      if (before.isDefined) {
        val after: Option[(Int, Int)] = pages.find((t: (Int, Int)) => t._1 == r._2)
        if (after.isDefined) {
          // both before and after pages specified in rule are found
          // get indices
          val bi = before.get._2
          val ai = after.get._2
          bi < ai
        } else {
          // after missing, no rule break
          true
        }
      }
      else {
        // before missing, no rule break
        true
      }
    }
  }

  private def middle(ps: PageSequence): Int = {
    assert((ps.length % 2) == 1)
    ps(ps.length/2)
  }

  def part1(input: Day05Input): Int = {
    input.pages.filter(inOrder(_, input.rules)).map(middle).sum
  }

  def mkOrdering(rules: Rules): Ordering[Int] = {
    new Ordering[Int] {
      def compare(x: Int, y: Int): Int = {
        if (x == y) {
          0
        } else {
          val wrong = rules.find((r: (Int, Int)) => x == r._1 && y == r._2)
          wrong match
            case Some(_) => -1
            case _ => 1
        }
      }
    }
  }

  def part2(input: Day05Input): Int = {
    input.pages
      .filter(!inOrder(_, input.rules))
      .map(ps => ps.sorted(mkOrdering(input.rules)))
      .map(middle)
      .sum
  }
}
