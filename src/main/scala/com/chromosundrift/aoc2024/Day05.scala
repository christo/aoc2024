package com.chromosundrift.aoc2024

import scala.io.Source

type Rules = Array[(Int, Int)]
type PageSequence = Array[Int]
case class Day05Input(rules: Rules, pages: Array[PageSequence])

object Day05 {
  def getInput(file: String): String = Source.fromResource(file).mkString

  def parseInput(s: String): Day05Input = {
    val Array(rulesPart, pagesPart) = s.split("\n\n")
    val rules = rulesPart.linesIterator.map(_.split('|').map(_.toInt) match {
      case Array(a, b) => (a, b)
    }).toArray
    val pages = pagesPart.linesIterator.map(_.split(",").map(_.toInt)).toArray
    Day05Input(rules, pages)
  }

  def main(args: Array[String]): Unit = {
    val sample = parseInput(getInput("day05_sample1.txt"))
    val input = parseInput(getInput("day05_input1.txt"))
    println(part1(sample))
    println(part1(input))
    println(part2(sample))
    println(part2(input))
  }

  private def inOrder(ints: PageSequence, rules: Rules): Boolean = {
    val pages = ints.zipWithIndex
    rules.forall { case (before, after) =>
      (for {
        (_, beforeIndex) <- pages.find(_._1 == before)
        (_, afterIndex)  <- pages.find(_._1 == after)
      } yield beforeIndex < afterIndex).getOrElse(true)
    }
  }

  private def middle(ps: PageSequence): Int = ps(ps.length / 2)

  def part1(input: Day05Input): Int = input.pages.filter(inOrder(_, input.rules)).map(middle).sum

  def part2(input: Day05Input): Int = {
    input.pages
      .filterNot(inOrder(_, input.rules))
      .map(ps => ps.sorted((x, y) => if (x == y) 0 else if (input.rules.contains((x, y))) -1 else 1))
      .map(middle)
      .sum
  }
}