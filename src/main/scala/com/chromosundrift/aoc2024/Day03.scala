package com.chromosundrift.aoc2024

import scala.io.Source

def getTestInput1 = {
  Source.fromResource("day03_input1.txt").mkString
}

object Day03 {
  def main(args: Array[String]): Unit = {
    val testInput = getTestInput1
    println(part1(testInput))
    println(part2(testInput))
  }

  def part1(str: String): Int = {
    val mulPattern = """mul\((\d{1,3}),(\d{1,3})\)""".r
    val products = mulPattern.findAllMatchIn(str).map { m =>
      val lhs = m.group(1).toInt
      val rhs = m.group(2).toInt
      lhs * rhs
    }
    products.sum
  }

  def part2(str: String): Int = {
    val mulPattern = """(mul)\((\d{1,3}),(\d{1,3})\)|(do)\(\)|(don't)\(\)""".r
    var enabled = true
    val products: Iterator[Int] = mulPattern.findAllMatchIn(str).map { m =>
      var product = 0
      if (enabled) {
        if (m.group(1) == "mul") {
          product = m.group(2).toInt * m.group(3).toInt
        } else if (m.group(5) == "don't") {
          enabled = false
        }
      } else if (m.group(4) == "do"){
        enabled = true
      }
      product
    }
    products.sum
  }
}
