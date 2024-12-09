package com.chromosundrift.aoc2024

import com.chromosundrift.aoc2024.Day09.{getInput, moveAll2, parse}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Spec extends AnyFlatSpec with Matchers {
  "getInput" should "fucking work" in {
    Day09.getInput("day09_sample1.txt") shouldBe "2333133121414131402"
  }

  "parse" should "get 3 files in tiny sample" in {
    val dm = Day09.parse("12345")
    dm.files.length shouldBe 3
  }

  it should "have sequential file indexes" in {
    val dm = Day09.parse("12345")
    dm.files(1).id shouldBe 1
  }

  it should "get 2 empties in tiny sample" in  {
    val dm = Day09.parse("12345")
    dm.empties.length shouldBe 2
  }

  it should "gather no bads" in {
    val dm = Day09.parse("2333133121414131402")
    dm.valid() shouldBe true
  }

  it should "not give bads for sample" in {
    parse(getInput("day09_sample1.txt")).valid() shouldBe true
  }

  it should "handle leading zeroes somehow" in {
    val fm = parse("0000004")
    fm.files(3).length shouldBe 4
  }

  "plot" should "match example" in {
    val dm = Day09.parse("2333133121414131402")
    dm.plot shouldBe "00...111...2...333.44.5555.6666.777.888899"
  }

  it should "handle 90909" in {
    val m = parse("90909")
    m.plot shouldBe "000000000111111111222222222"
  }

  it should "handle leading zeroes in diskmap" in {
    val fm = parse("00000049")
    fm.plot shouldBe "3333........."
  }

  "move1" should "match first basic example" in {
    Day09.move1("0..111....22222") shouldBe Some("02.111....2222.")
  }

  it should "not move already compacted shit" in {
      Day09.move1("3333.........") shouldBe None
  }

  "moveAll" should "match sample" in {
    val input = "00...111...2...333.44.5555.6666.777.888899"
    val expected = "0099811188827773336446555566.............."
    Day09.moveAll(input) shouldBe expected
  }

}
