package com.chromosundrift.aoc2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day08Spec extends AnyFlatSpec with Matchers {
  "antinodes part 1" should "get basic shit" in {
    val antinodes = Day08.antinodes1(List((2, 2), (4, 4)), _ => true).toSet
    antinodes shouldBe Set((0, 0), (6, 6))
  }

  it should "do horizontal" in {
    val antinodes = Day08.antinodes1(List((2, 2), (4, 2)), _ => true)
    val expected = Set((0, 2), (6, 2))
    antinodes.toSet shouldBe expected
  }
}
