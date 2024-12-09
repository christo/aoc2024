package com.chromosundrift.aoc2024

import scala.io.Source

//noinspection ScalaWeakerAccess
object Day08 {
  type V2 = (Int, Int)

  def getInput(file: String): Array[String] = Source.fromResource(file).getLines.toArray

  val antennaRx = """([A-Za-z0-9])""".r

  def main(args: Array[String]): Unit = {
    val rows = getInput("day08_input1.txt")
    val antennaMap: Map[Char, List[(Int, Int)]] = parse(rows)
    println("part1")
    part1(rows, antennaMap)
    // part two is geborked right now and gives the wrong answer:
    println("part2")
    part2(rows, antennaMap)
  }

  def part1(rows: Array[String], antennaMap: Map[Char, List[V2]]): Unit = {
    val filter = onMap(rows)
    val antiNodes = antennaMap.map(f => (f._1, antinodes(f._2, 1).filter(filter))).values.flatten.toSet
    println(antiNodes.size)
  }

  def part2(rows: Array[String], antennaMap: Map[Char, List[V2]]): Unit = {
    val filter = onMap(rows)

    @annotation.tailrec
    def loop(locs: List[(Int, Int)], multiplier: Int, acc: List[V2]): List[V2] = {
      val result = antinodes(locs, multiplier).filter(filter)
      if (result.isEmpty) {
        println(s"max multiplier reached: ${multiplier}")
        acc
      }
      else loop(locs, multiplier + 1, acc ++ result)
    }

    // collect all antinodes, increasing scale of delta until no results are returned
    // (because all more distant antinodes are off-map)
    val antinodesByFreq = antennaMap.map(f => (f._1, loop(f._2, 1, List.empty)))
    plot(rows, antinodesByFreq)
    println(antinodesByFreq.values.flatten.toSet.size)
  }

  def onMap(rows: Array[String]): V2 => Boolean = {
    (r, c) => r >= 0 && rows.indices.contains(r) && rows(0).indices.contains(c)
  }

  def antinodes(locs: List[V2], n: Int): List[(V2)] = {
    locs.flatMap(i => locs.filter(_ != i).map(j => (i, j)))
      .flatMap((a: V2, b: V2) => extend(a, b, n))
  }

  private def extend(a: V2, b: V2, n: Int) = {
    val (aRow, aCol) = a
    val (bRow, bCol) = b
    val (deltaRow, deltaCol) = ((bRow - aRow) * n, (bCol - aCol) * n)
    List((aRow - deltaRow, aCol - deltaCol), (bRow + deltaRow, bCol + deltaCol))
  }

  private def dump(antennaMap: Map[Char, List[V2]]): Unit = {
    antennaMap.map((byFreq: (Char, List[V2])) => {
      val freqChar: Char = byFreq._1
      s"$freqChar: ${byFreq._2.mkString(",")}"
    }).foreach(println)
  }

  /**
   * Plot a the originalMap with antennas on it
   * @param originalMap
   * @param antennaMap
   */
  def plot(originalMap: Array[String], antennaMap: Map[Char, List[V2]]): Unit = {
    println("PLOT:")
    originalMap.zipWithIndex.foreach { (row: String, r: Int) =>
      row.zipWithIndex.foreach { (cell: Char, c: Int) =>
        antennaMap.map { (f, loc) =>
          if ((cell == '.' || cell == '#') && loc.contains((r, c))) print('$') else print(cell)
        }
      }
      println()
    }

  }

  def parse(rows: Array[String]): Map[Char, List[V2]] = {
    val antennas = for {
      r <- rows.indices
      row = rows(r)
      matchResult <- antennaRx.findAllMatchIn(row)
    } yield {
      val freq: Char = matchResult.group(0).head
      val c = matchResult.start
      (freq, r, c)
    }
    antennas.groupBy(_._1)
      .view
      .mapValues(pairs => pairs.map(t => (t._2, t._3)).toList)
      .toMap
      .filter(_._2.length >= 2)
  }

}
