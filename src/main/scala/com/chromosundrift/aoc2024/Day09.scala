package com.chromosundrift.aoc2024

import scala.io.Source
import scala.util.matching.Regex

object Day09 {
  private val degragRx: Regex = """^(\d*)\.(.*)(\d)(\.*)$""".r

  def getInput(file: String): String = Source.fromResource(file).mkString

  def main(args: Array[String]): Unit = {
    val input = getInput("da09_input1.txt").trim
    if (!input.matches("[\\d\\.]+")) {
      throw new IllegalStateException("input invalid")
    }
    val m = parse(input)
    
    val finalPlot = moveAll2(m.seq)
    println(finalPlot)
    val result = checksum(finalPlot)
    println(result)

  }

  def parse(diskmap: String): DiskMap = {
    // deinterleave fileblock and emptyblock
    val (file, free) = diskmap.map(_.asDigit).zipWithIndex.partition { case (_, index) => index % 2 == 0 }

    // reindex files to get correct id after deinterleaving
    DiskMap(file.map(_._1).zipWithIndex.map(t => FileBlock(t._2, t._1)), free.map(_._1))
  }


  def checksum(seq: List[Long]): Long = {
    assert(seq.min >= -1)
    seq.filter(_ >= 0).zipWithIndex.map { (digit: Long, index: Int) =>
      val product: Long = digit * index.toLong
      //      println(s"${index} * ${digit} = ${product}")
      product
    }.sum
  }

  def moveAll2(plot: List[Long]): List[Long] = {
    var left = 0
    var right = plot.length-1
    val plotArray = plot.toArray
    while (left < right) {
      // only move 1 at a time
      if (plotArray(left) != -1L) {
        left+= 1
      } else if (plotArray(right) == -1L) {
        right -= 1
      } else {
        // swap dot on left with number on right
        val toMove = plotArray(right)
        plotArray(right) = plotArray(left)
        plotArray(left) = toMove
      }
    }
    plotArray.toList
  }

  @deprecated
  def moveAll(plot: String): String = {
    @annotation.tailrec
    def loop(currentPlot: String, lastValid: Option[String]): Option[String] = {
      move1(currentPlot) match {
        case some@Some(newPlot) => loop(newPlot, some)
        case None => lastValid
      }
    }

    loop(plot, Some(plot)).get
  }

  @deprecated
  def move1(plot: String): Option[String] = {
    // group 1 fixed leading file blocks
    // group 2 anything before digit to move
    // group 3 digit to move (non-empty)
    // group 4 any trailing free space
    val result = degragRx.replaceAllIn(plot, m => s"${m.group(1)}${m.group(3)}${m.group(2)}.${m.group(4)}")
//    println(s"input: ${plot}")
//    println(s"outpt: ${result}")
//    println()
    if plot.equals(result) then None else Some(result)

  }
}

case class FileBlock(id: Int, length: Int) {
  override def toString: String = s"${id}:${length}"
}

case class DiskMap(files: IndexedSeq[FileBlock], empties: IndexedSeq[Int]) {

  def valid(): Boolean = {
    val bads = files.filter(_.length < 0).map(fb => s"${fb.id} length ${fb.length}")
//    bads.foreach(println)
    bads.length == 0
  }

  def dump: String = {
    val fs = files.map { f =>
      s"${f.id}:${f.length}"
    }.mkString(",")
    val es = empties.mkString(",")
    s"files>${fs}\nempties>${es}"
  }

  @deprecated
  def plot: String = {
    files.map(file => {
      val filePlot = file.id.toString * file.length
      if (filePlot.length > file.length) {
        throw new IllegalStateException("nono")
      }
      val freePlot = Option.when(empties.indices.contains(file.id))("." * empties(file.id)).getOrElse("")
      s"$filePlot$freePlot"
    }).mkString
  }

  def seq: List[Long] = {
    val blurt: Seq[List[Long]] = files.map(file => {
      val filePlot = List.fill(file.length)(file.id.toLong)
      if (filePlot.length > file.length) {
        throw new IllegalStateException("nono")
      }
      val freePlot = Option.when(empties.indices.contains(file.id.toLong))(List.fill(empties(file.id))(-1L)).getOrElse(List.empty)
      filePlot.concat(freePlot)
    })
    blurt.flatten.toList
  }
}
