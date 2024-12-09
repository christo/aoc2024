package com.chromosundrift.aoc2024

import com.chromosundrift.aoc2024.Day06.{collect, getInput, guardLoops, parse, part1, part2, step}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day06Spec extends AnyFlatSpec with Matchers {

  "part1" should "pass sample" in {
    part1(parse(getInput("day06_sample1.txt"))) shouldBe 41
  }

  it should "pass for main input " in {
    part1(parse(getInput("day06_input1.txt"))) shouldBe 5208
  }

  "part2" should "pass sample" in {
    part2(parse(getInput("day06_sample1.txt"))) shouldBe 6
  }

  it should "pass for main input" in {
    pending
    part2(parse(getInput("day06_input1.txt"))) shouldBe 1972
  }

  "guardLoops" should "match example" in {
    val sample1: GuardMap = parse(getInput("day06_sample1.txt"))
    val withObstacle = GuardMap(sample1.rows, sample1.guard, 0, Some((6, 3)))
    guardLoops(withObstacle) shouldBe true
  }

  "collect" should "match example" in {
    val sample1: GuardMap = parse(getInput("day06_sample1.txt"))
    val withObstacle = GuardMap(sample1.rows, sample1.guard, 0, Some((6, 3)))
    val collection = collect(withObstacle)
    println(collection.map(_.steps).max)
    collection.foreach(gm => println(gm))
    collection.map(_.steps).max should be > 10
  }

  "tuple" should "have sane equality" in {
    val t1 = (1, 33)
    val t2 = (1, 33)
    t1 shouldEqual t2
  }

  it should "work through option contains" in {
    val t1 = (1, 33)
    val t2 = (1, 33)
    Some(t1).contains(t2) shouldBe true
  }

}
