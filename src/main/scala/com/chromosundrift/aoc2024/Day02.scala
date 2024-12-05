package com.chromosundrift.aoc2024

import scala.io.Source

object Day02 {

  def getTestInput1: List[String] = Source.fromResource("day02_input1.txt").getLines().toList

  def main(args: Array[String]): Unit = {
    val reports = getTestInput1.map(ints)
    println(part1(reports))
    println(part2(reports))
  }

  def part2(reports: List[List[Int]]): Int = {
    reports.count(report => allSafe(report) || tryRemoving1(report).isDefined)
  }

  private def tryRemoving1(report: List[Int]): Option[Int] = {
    for (i <- report.indices) {
      val oneRemoved = report.zipWithIndex.filter(_._2 != i)
      if (allSafe(oneRemoved.map(_._1))) return Some(i)
    }
    None
  }

  def part1(reports: List[List[Int]]): Int = reports.count(allSafe)

  def ints(line: String): List[Int] = line.split("\\s+").map(_.toInt).toList

  def monotonic(ds: List[Int]): Boolean = ds.map(_.sign).distinct.size <= 1

  def allSafe(report: List[Int]): Boolean = {
    val ds = deltas(report)
    monotonic(ds) && ds.forall(inRange)
  }

  def deltas(levels: List[Int]): List[Int] =
    levels.sliding(2).map(pairs => pairs(1) - pairs(0)).toList

  def inRange(delta: Int): Boolean = (1 to 3).contains(Math.abs(delta))

}