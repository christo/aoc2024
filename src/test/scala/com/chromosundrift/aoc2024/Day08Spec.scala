package com.chromosundrift.aoc2024

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Ad hoc tests only to actual find bugs encountered.
 */
class Day08Spec extends AnyFlatSpec with Matchers {
  "antinodes part 1" should "get basic shit" in {
    val antinodes = Day08.antinodes(List((2, 2), (4, 4)), 1).toSet
    antinodes shouldBe Set((0, 0), (6, 6))
  }

  it should "do horizontal" in {
    val antinodes = Day08.antinodes(List((2, 2), (4, 2)), 1)
    val expected = Set((0, 2), (6, 2))
    antinodes.toSet shouldBe expected
  }
}
