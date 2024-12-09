package com.chromosundrift.aoc2024

import scala.collection.immutable
import scala.io.Source

//noinspection ScalaWeakerAccess
object Day08 {
  type V2 = (Int, Int)

  def getInput(file: String): Array[String] = Source.fromResource(file).getLines.toArray

  val antennaRx = """([A-Za-z0-9])""".r

  def main(args: Array[String]): Unit = {
    val rows = getInput("day08_input1.txt")
    val locOnMap = onMap(rows)
    val antennaMap: Map[Char, List[(Int, Int)]] = parse(rows)
    println("part1")
    println(part1(locOnMap, antennaMap))
    println("part2")
    println(part2(locOnMap, antennaMap))
  }

  /**
   * Find antinodes using a fixed distance multiplier of 1
   */
  def part1(filter: V2 => Boolean, antennaMap: Map[Char, List[V2]]): Int = {
    val multiplier = 1
    antennaMap.map(f => (f._1, antinodes(f._2, multiplier).filter(filter))).values.flatten.toSet.size
  }

  /**
   * Find antinodes using increasing distance multiplier until no antinodes match filter
   */
  def part2(filter: V2 => Boolean, antennaMap: Map[Char, List[V2]]): Int = {

    @annotation.tailrec
    def loop(locs: List[(Int, Int)], multiplier: Int, acc: List[V2]): List[V2] = {
      val result = antinodes(locs, multiplier).filter(filter)
      if (result.isEmpty) acc
      else loop(locs, multiplier + 1, acc ++ result)
    }

    antennaMap.map(f => (f._1, loop(f._2, 0, List.empty))).values.flatten.toSet.size
  }

  /**
   * @param rows square map
   * @return a predicate returning true iff a given location is on the map
   */
  def onMap(rows: Array[String]): V2 => Boolean = {
    (r, c) => r >= 0 && rows.indices.contains(r) && rows(0).indices.contains(c)
  }

  /**
   * Find antinodes for antennas at locs using antenna distance multiplier n
   */
  def antinodes(locs: List[V2], n: Int): List[(V2)] = {
    locs.flatMap(i => locs.filter(_ != i).map(j => (i, j)))
      .flatMap((a: V2, b: V2) => extend(a, b, n))
  }

  /**
   * Bidirectional colinear extension of the a,b segment multiplied by harmonic scale
   */
  private def extend(a: V2, b: V2, scale: Int) = {
    val (aRow, aCol) = a
    val (bRow, bCol) = b
    val (deltaRow, deltaCol) = ((bRow - aRow) * scale, (bCol - aCol) * scale)
    List((aRow - deltaRow, aCol - deltaCol), (bRow + deltaRow, bCol + deltaCol))
  }

  /**
   * Print a summary of the antennas found for each frequency on the given antennaMap
   */
  private def dump(antennaMap: Map[Char, List[V2]]) = antennaMap.map((byFreq: (Char, List[V2])) => {
      val freqChar: Char = byFreq._1
      s"$freqChar: ${byFreq._2.mkString(",")}"
    }).foreach(println)


  /**
   * Plot the originalMap with marked overlay positions on it (preserving antenna locations)
   * @param originalMap provided puzzle input
   * @param overlay locations by frequency to plot on top of original map
   */
  def plot(originalMap: Array[String], overlay: Map[Char, List[V2]]): Unit = {
    println("PLOT:")
    val composite = originalMap.zipWithIndex.map { case (row, rowIdx) =>
      val chars = row.zipWithIndex.map { case (cell, colIdx) =>
        if ((cell == '.' || cell == '#') &&
          overlay.values.flatten.toList.contains((rowIdx, colIdx))) '$'
        else cell
      }
      chars.mkString
    }
    composite.foreach(println)
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
      .filter(_._2.length >= 2)  // only use freq antennas numbering at least two
  }

}
