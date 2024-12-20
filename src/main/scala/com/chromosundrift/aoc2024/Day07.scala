package com.chromosundrift.aoc2024
import scala.io.Source

case class Op(symbol: String, fn: (Long, Long) => Long)
type Eqn = (Long, Array[Long])

object Day07 {
  def main(args: Array[String]): Unit = {
    val eqns: Array[Eqn] = parse(getInput("day07_input1.txt"))
    println("part 1")
    solve(eqns, part1Ops)
    println("part 2")
    solve(eqns, part2Ops)
  }

  private def solve(eqns: Array[(Long, Array[Long])], ops: Array[Op]): Unit = {
    var sumOfValidTargets = 0L

    eqns.foreach { eqn =>
      val attempts = validateEquation(eqn, ops)

      if (attempts.exists(_._4 == eqn._1)) {
        val successful = attempts.find(_._4 == eqn._1).get
        sumOfValidTargets += eqn._1 // Add target value to sum when equation is valid
      }
    }

    println(s"Total valid equations: ${eqns.count(e => validateEquation(e, part2Ops).exists(_._4 == e._1))}")
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

  private def validateEquation(eqn: (Long, Array[Long]), ops: Array[Op]): Array[(Long, String, Long, Long, String)] = {
    val numbers = eqn._2

    if (numbers.length < 2) return Array.empty

    val requiredOps = numbers.length - 1

    ops.flatMap { firstOp =>
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

      finalResults.map(state =>
        (numbers(0), firstOp.symbol, numbers(1), state.acc, state.expr)
      )
    }
  }
}
