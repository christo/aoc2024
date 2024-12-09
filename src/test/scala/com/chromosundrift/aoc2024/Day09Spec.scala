package com.chromosundrift.aoc2024

import com.chromosundrift.aoc2024.Day09.{part1, part2, checksumPart2, defragPart1, defragPart2, getInput, parsePart1, parsePart2, plotSimple}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day09Spec extends AnyFlatSpec with Matchers {
  private val SAMPLE = "2333133121414131402"
  "getInput" should "fucking work" in {
    getInput("day09_sample1.txt") shouldBe SAMPLE
  }

  "parse" should "get 3 files in tiny sample" in {
    val dm = parsePart1("12345")
    dm.files.length shouldBe 3
  }

  it should "have sequential file indexes" in {
    val dm = parsePart1("12345")
    dm.files(1).id shouldBe 1
  }

  it should "get 2 empties in tiny sample" in  {
    val dm = parsePart1("12345")
    dm.empties.length shouldBe 2
  }

  it should "gather no bads" in {
    val dm = parsePart1(SAMPLE)
    dm.valid() shouldBe true
  }

  it should "not give bads for sample" in {
    parsePart1(getInput("day09_sample1.txt")).valid() shouldBe true
  }

  it should "handle leading zeroes somehow" in {
    val fm = parsePart1("0000004")
    fm.files(3).length shouldBe 4
  }

  "checksumPart2" should "match sample" in {
    val blocks = parsePart2(SAMPLE)
    val defragged = defragPart2(blocks)
//    println(s"defragged fileblocks: ${defragged}")
    checksumPart2(defragged) shouldBe 2858L
  }

  "parsePart2" should "have correct ids" in {
    val blocks = parsePart2(SAMPLE)
    blocks.map(_.id).max shouldBe 9
  }

  it should "have samplePlot matching sample" in {
    val blocks = parsePart2(SAMPLE)
    plotSimple(blocks) shouldBe "00...111...2...333.44.5555.6666.777.888899"
  }

  "defragPart2" should "match sample" in {
    val expectedSimplePlot = "00992111777.44.333....5555.6666.....8888.."
    val blocks = parsePart2(SAMPLE)
    val defragged = defragPart2(blocks)
    val defragPlot = plotSimple(defragged)
    defragPlot shouldBe expectedSimplePlot
  }

  "part1" should "solve puzzle input" in {
    val input = getInput("day09_input2.txt").trim
    part1(input) shouldBe 6344673854800L
  }
  
  "part2" should "solve my puzzle input" in {
    val input = getInput("day09_input2.txt").trim
    part2(input) shouldBe 6360363199987L
  }

}
