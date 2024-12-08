package com.chromosundrift.aoc2024
import scala.io.Source

case class Op(symbol: String, fn: (Long, Long) => Long)
type Eqn = (Long, Array[Long])

object Day07 {
  def main(args: Array[String]): Unit = {
//    val eqns: Array[Eqn] = parse(getInput("day07_sample1.txt"))
    val eqns: Array[Eqn] = parse(getInput("day07_input1.txt"))
    var sumOfValidTargets = 0L

    eqns.foreach { eqn =>
      val attempts = validateEquation(eqn)
//      println(s"Target: ${eqn._1}, Numbers: ${eqn._2.mkString(", ")}")

      if (attempts.exists(_._4 == eqn._1)) {
        val successful = attempts.find(_._4 == eqn._1).get
//        println(s"Valid: true")
//        println(s"Found: ${successful._5}")
        sumOfValidTargets += eqn._1  // Add target value to sum when equation is valid
      } else {
//        println("Valid: false")
//        println("Attempted calculations:")
//        attempts.foreach { case (_, _, _, result, expression) =>
//          println(s"  $expression = $result")
//        }
      }
//      println("-" * 40)
    }

    println(s"Total valid equations: ${eqns.count(e => validateEquation(e).exists(_._4 == e._1))}")
    println(s"Sum of valid equation targets: $sumOfValidTargets")
  }

  private val MUL = Op("*", (l, r) => l * r)
  private val ADD = Op("+", (l, r) => l + r)
  private val CAT = Op("||", (l, r) => s"${l.toString}${r.toString}".toLong)
  val part1Ops = Array(MUL, ADD)
  val part2Ops = Array(MUL, ADD, CAT)

  def getInput(file: String): Array[String] = Source.fromResource(file).getLines.toArray

  def parse(lines: Array[String]): Array[Eqn] = {
    lines.map { l =>
      val tokens = l.split("[:\\s]+").map(_.toLong)
      (tokens.head, tokens.tail)
    }
  }

  private case class CalcState(acc: Long, expr: String)

  private def validateEquation(eqn: Eqn): Array[(Long, String, Long, Long, String)] = {
    val numbers = eqn._2

    if (numbers.length < 2) return Array.empty

    // We always need numbers.length - 1 operations to use all numbers
    val requiredOps = numbers.length - 1

    part2Ops.flatMap { firstOp =>
      val initial = CalcState(firstOp.fn(numbers(0), numbers(1)),
        s"${numbers(0)} ${firstOp.symbol} ${numbers(1)}")

      val finalResults = numbers.drop(2).zipWithIndex.foldLeft(Array(initial)) { case (states, (nextNum, idx)) =>
        states.flatMap { state =>
          part2Ops.map { op =>
            val newResult = op.fn(state.acc, nextNum)
            val newExpr = s"(${state.expr}) ${op.symbol} $nextNum"
            CalcState(newResult, newExpr)
          }
        }
      }

      // Only return results that used all operations (and thus all numbers)
      finalResults.map(state =>
        (numbers(0), firstOp.symbol, numbers(1), state.acc, state.expr)
      )
    }
  }
}
