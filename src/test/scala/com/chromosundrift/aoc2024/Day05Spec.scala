package com.chromosundrift.aoc2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Spec extends AnyFlatSpec with Matchers {

  "day05 part 1" should "pass sample input" in {
    Day05.part1(Day05.parseInput(Day05.getInput("day05_sample1.txt"))) shouldBe 143
  }

  it should "pass test input" in {
    Day05.part1(Day05.parseInput(Day05.getInput("day05_input1.txt"))) shouldBe 4462
  }

  "day05 part 2" should "pass sample input" in {
    Day05.part2(Day05.parseInput(Day05.getInput("day05_sample1.txt"))) shouldBe 123
  }

  it should "pass test input" in {
    Day05.part2(Day05.parseInput(Day05.getInput("day05_input1.txt"))) shouldBe 6767
  }
}
