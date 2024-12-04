package com.chromosundrift.aoc2024

import scala.io.Source


object Day01 {
  def main(args: Array[String]): Unit = {
    val (left, right) = Source.fromResource("day01_input1.txt").getLines().map { line =>
      val parts = line.split("\\s+")
      (parts(0).toInt, parts(1).toInt)
    }.toList.unzip
    val lSorted = left.sorted
    val rSorted = right.sorted
    part01(lSorted zip rSorted)
    part02(lSorted, rSorted)
  }

  private def part01(sortedPairs: List[(Int, Int)]): Unit = {
    val sum = sortedPairs.map((p: (Int, Int)) => Math.abs(p._1 - p._2)).sum
    println(sum)
  }

  private def part02(left: List[Int], right: List[Int]): Unit = {
    val sum = left.map(v => v * right.count(i => i == v)).sum
    println(sum)
  }
}