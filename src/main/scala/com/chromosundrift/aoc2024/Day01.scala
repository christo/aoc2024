package com.chromosundrift.aoc2024

import scala.io.Source

//noinspection ScalaWeakerAccess
object Day01 {
  def main(args: Array[String]): Unit = {
    val (left, right) = Source.fromResource("day01_input1.txt").getLines()
      .map(_.split("\\s+").map(_.toInt))
      .map { case Array(l, r) => (l, r) }
      .toList.unzip
    val lSorted = left.sorted
    val rSorted = right.sorted
    println(part01(lSorted zip rSorted))
    println(part02(lSorted, rSorted))
  }

  def part01(sortedPairs: List[(Int, Int)]): Int = sortedPairs.map(p => (p._1 - p._2).abs).sum

  def part02(left: List[Int], right: List[Int]): Int = left.map(v => v * right.count(_ == v)).sum
}