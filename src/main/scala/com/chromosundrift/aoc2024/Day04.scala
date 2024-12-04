package com.chromosundrift.aoc2024

import scala.io.Source

type DeltaSpec = Seq[List[(Int, Int)]]

object Day04 {
  private val part1Sample =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin

  private val part2Sample =
    """.M.S......
      |..A..MSMS.
      |.M.S.MAA..
      |..A.ASMSM.
      |.M.S.M....
      |..........
      |S.S.S.S.S.
      |.A.A.A.A..
      |M.M.M.M.M.
      |..........""".stripMargin

  val part1SampleOccurrences = 18
  private val part1Targets = List("XMAS", "SAMX")
  // only need to handle half the directions: LtR, TLtBR, TtB, LRtBL
  // because we search for both forwards and backwards words
  private val part1Deltas: DeltaSpec = List(
    List((0, 0), (0, 1), (0, 2), (0, 3)), // left to right
    List((0, 0), (1, 1), (2, 2), (3, 3)), // top left to bottom right
    List((0, 0), (1, 0), (2, 0), (3, 0)), // top to bottom
    List((0, 0), (1, -1), (2, -2), (3, -3)), // top right to bottom left
  )

  private val part2Targets = List("MASMAS", "MASSAM", "SAMMAS", "SAMSAM")
  private val part2Deltas: DeltaSpec = List(
    List((0, 0), (1, 1), (2, 2), (0, 2), (1, 1), (2, 0)), // TLtBR,BLtTR
  )

  def getTestInput1: String = {
    Source.fromResource("day04_input1.txt").mkString
  }

  def main(args: Array[String]): Unit = {
    val input = getTestInput1
    println(solve(part1Targets, part1Deltas, part1Sample))
    println(solve(part1Targets, part1Deltas, input))
    println(solve(part2Targets, part2Deltas, part2Sample))
    println(solve(part2Targets, part2Deltas, input))
  }

  private def solve(targets: List[String], deltas: DeltaSpec, input: String): Int = {
    var wordCount = 0
    val rows = input.split('\n')

    // we assume for now we have rectangle puzzle

    assert(rows.length > 0)
    val rowLength = rows(0).length
    assert(rows.forall(r => r.length == rowLength))

    for (j <- rows.indices) {
      val row = rows(j)
      for (i <- row.indices) {
        // both can't succeed but code is simpler this way
        wordCount += targets.map(t => findWords(t, rows, i, j, deltas)).sum
      }
    }
    wordCount
  }

  private def findWords(needle: String, haystack: Array[String], i: Int, j: Int, deltas: DeltaSpec): Int = {
    var wordCount = 0
    deltas.foreach { strip =>
      var found = true
      for (index <- needle.indices) {
        val letterDelta = strip(index)
        val jj = j + letterDelta._1
        val ii = i + letterDelta._2
        if (jj < 0 || jj >= haystack.length || ii < 0 || ii >= haystack(jj).length) {
          // out of bounds
          found = false
        } else {
          val findLetter = needle(index)
          if (needle(index) != haystack(jj)(ii)) {
            found = false
          }
        }
      }
      if (found) {
        wordCount += 1
      }
    }
    wordCount
  }
}
