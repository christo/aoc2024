package com.chromosundrift.aoc2024

import scala.io.Source


object Day02 {

  def dump[A](x: List[A]): Unit = {
    println(x.mkString(", "))
  }

  def main(args: Array[String]): Unit = {
    val reports = getTestInput1
    println(part1(reports))
    println(part2(reports))
  }

  def getTestInput1: List[String] = {
    Source.fromResource("day02_input1.txt").getLines().toList
  }

  def part1(lines: List[String]): Int = {
    val reports = lines.map(ints)
    val ds = reports.map(x => deltas(x))
    val isSafes = ds.map { (x: List[Int]) =>
      val isMonotonic = monotonic(x)
      val allInRange = x.map(inRange).forall(identity)
      isMonotonic && allInRange
    }
    isSafes.count(identity)
  }

  def monotonic(ds: List[Int]): Boolean = {
    consistentSigns(ds).forall(identity)
  }

  def part2(lines: List[String]): Int = {
    val is: Seq[List[Int]] = lines.map(a => ints(a))
    val safes: Seq[Boolean] = is.map(report => part2SafeCount(report))
    safes.count(identity)
  }

  private def part2SafeCount(report: List[Int]): Boolean = {
    allSafe(report) || tryRemoving1(report).isDefined
  }

  private def tryRemoving1(report: List[Int]): Option[Int] = {
    for (i <- report.indices) {
      // remove one
      val oneRemoved = report.zipWithIndex.filter(_._2 != i)
      if (allSafe(oneRemoved.map(_._1))) return Some(i)
    }
    None
  }

  def part1Alt(lines: List[String]): Int = {
    val reports = lines.map(ints)
    val safeList = reports.map(allSafe)
    // return the number of safe reports
    safeList.count(identity)
  }

  def ints(line: String): List[Int] = {
    line.split("\\s+").map(x => x.toInt).toList
  }

  def allSafe(report: List[Int]): Boolean = {
    val ds = deltas(report)
    val monotonics: Seq[Boolean] = consistentSigns(ds)
    val inRanges: Seq[Boolean] = ds.map(inRange)
    monotonics.forall(identity) && inRanges.forall(identity)
  }

  def deltas(levels: List[Int]): List[Int] = {
    levels.sliding(2).map(pairs => pairs(1) - pairs(0)).toList
  }

  def inRange(delta: Int): Boolean = {
    val abs = Math.abs(delta)
    abs >= 1 && abs <= 3
  }

  private def consistentSigns(ds: List[Int]): List[Boolean] = {
    ds.sliding(2).map(ds => ds(0).sign == ds(1).sign).toList
  }
}