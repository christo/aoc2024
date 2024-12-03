package com.chromosundrift.aoc2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Spec extends AnyFlatSpec with Matchers {

  val testInput: List[String] = List(
    "7 6 4 2 1",  // safe
    "1 2 7 8 9",  // jump up too big
    "9 7 6 2 1",  // jump down too big
    "1 3 2 4 5",  // change of direction - nonmonotonic
    "8 6 4 4 1",  // no change disallowed
    "1 3 6 7 9",  // safe
  )


  "day02 part 1" should "give whole correct test output" in {
    Day02.part1(testInput) shouldBe 2
  }

  it should "give whole correct test output for alt" in {
    Day02.part1Alt(testInput) shouldBe 2
  }

  it should "work with one record" in {
    val safeRecord = testInput.take(1)
    safeRecord shouldBe List("7 6 4 2 1")
    Day02.part1Alt(safeRecord) shouldBe 1
  }

  it should "work with nonmonotonic record" in {
    Day02.part1Alt(List(testInput(3))) shouldBe 0
  }

  "safes" should "work with test input" in {
    val records: Seq[List[Int]] = testInput.map(Day02.ints)
    val safes: Seq[Boolean] = records.map(Day02.allSafe)
    safes shouldBe Seq(true, false, false, false, false, true)
  }

  it should "show first test input as safe" in {
    val ints = Day02.ints(testInput(0))
    Day02.allSafe(ints) shouldBe true
  }


  "internal functions" should "detect monotonic sequences" in {
    val deltas = List(1,4,5,1,3,5)
    Day02.monotonic(deltas) shouldBe true
  }

  it should "handle all negative deltas as monotonic sequences" in {
    val deltas = List(-1,-4,-5,-1,-3,-5)
    Day02.monotonic(deltas) shouldBe true
  }

  it should "not show nonmonotonic as monotonic" in {
    val deltas = List(1, 4, -5, 1, 3, 5)
    Day02.monotonic(deltas) shouldBe false
  }

  it should "treat zero deltas as fail" in {
    val deltas = List(1, 4, 0, 1, 3, 5)
    Day02.monotonic(deltas) shouldBe false
  }

  it should "calculate deltas correctly" in {
    val numbers = List(1, 2, 7, 8, 9)
    val deltas = Day02.deltas(numbers).toList
    deltas shouldBe List(1, 5, 1, 1)
  }
  it should "calculate zero deltas correctly" in {
    val numbers = List(8, 6, 4, 4, 1)
    val deltas = Day02.deltas(numbers).toList
    deltas shouldBe List(-2, -2, 0, -3)
  }

  it should "pass test input for range" in {
    val records: Seq[List[Int]] = testInput.map(Day02.ints)
    val deltas = records.map(x => (x, Day02.deltas(x.toList).toList))

    val inRanges = deltas.map { (x: List[Int], ds: List[Int]) =>
      val bs: List[Boolean] = ds.map((d: Int) => Day02.inRange(d)).toList
      bs.length shouldBe ds.length
      bs.length shouldBe (x.length - 1)
      !bs.contains(false)
    }.toList
    inRanges shouldBe List(true, false, false, true, false, true)
  }

  it should "pass test input for monotonicity" in {
    val ints = testInput.map(Day02.ints)
    val deltas = ints.map(x => (x, Day02.deltas(x.toList)))
    val monos = deltas.map { (x, ds) =>
      val isMonotonic = Day02.monotonic(ds)
      isMonotonic
    }.toList
    monos shouldBe List(true, true, true, false, false, true)
  }

  "Day02" should "solve part 1" in {
    Day02.part1(Day02.getTestInput1) shouldBe 371
  }

  it should "solve part 1 Alt" in {
    Day02.part1Alt(Day02.getTestInput1) shouldBe 371
  }

  it should "pass test input part 2" in {
    Day02.part2(testInput) shouldBe 4
  }
}
