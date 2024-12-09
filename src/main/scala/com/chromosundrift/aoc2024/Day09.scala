package com.chromosundrift.aoc2024

import scala.io.Source
import scala.util.matching.Regex

//noinspection ScalaWeakerAccess
object Day09 {
  private val degragRx: Regex = """^(\d*)\.(.*)(\d)(\.*)$""".r

  def getInput(file: String): String = Source.fromResource(file).mkString

  def main(args: Array[String]): Unit = {
    val input = getInput("day09_input2.txt").trim
    if (!input.matches("[\\d\\.]+")) {
      throw new IllegalStateException("input invalid")
    }
    println("part 1")
    println(part1(input))
    println("part 2")
    println(part2(input))
  }


  def part1(input: String) = {
    checksumPart1(defragPart1(parsePart1(input).seq))
  }

  def part2(input: String) = {
    checksumPart2(defragPart2(parsePart2(input)))
  }

  def checksumPart2(files: Seq[FileBlock]): Long = {
    files.flatMap(f => List.fill(f.length)(f.id.toLong))
      .zipWithIndex.map((id, index) => if (id >= 0) id * index else 0).sum
  }

  def parsePart1(diskmap: String): DiskMap = {
    // deinterleave fileblock and emptyblock
    val (file, free) = diskmap
      .map(_.asDigit)
      .zipWithIndex
      .partition { case (_, index) => index % 2 == 0 }

    // reindex files to get correct id after deinterleaving
    DiskMap(file.map(_._1).zipWithIndex.map(t => FileBlock(t._2, t._1)), free.map(_._1))
  }

  /**
   * List of variable size fileblocks which may be empty
   */
  def parsePart2(diskmap: String): IndexedSeq[FileBlock] = {
    diskmap.map(_.asDigit).zipWithIndex.map { (len, index) =>
      if (index % 2 == 0) {
        FileBlock(index / 2, len)
      } else {
        FileBlock(-1, len)
      }
    }
  }

  def checksumPart1(seq: List[Int]): Long = {
    assert(seq.min >= -1) // sanity
    // do not use empty blocks (-1)
    seq.filter(_ >= 0).zipWithIndex.map(_ * _.toLong).sum
  }


  def defragPart1(blocks: List[Int]): List[Int] = {
    // two indices counting from each direction to swap
    var left = 0
    var right = blocks.length - 1
    val plotArray = blocks.toArray
    while (left < right) {
      // only move 1 at a time
      if (plotArray(left) != -1) {
        left += 1
      } else if (plotArray(right) == -1) {
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


  def defragPart2(files: IndexedSeq[FileBlock]): IndexedSeq[FileBlock] = {
    // files sorted by id, asc with single empty blocks
    val fileBlocks = files.filter(!_.isEmpty()).sortBy(_.id)(Ordering[Int].reverse)

    fileBlocks.foldLeft(files) { (currentBlocks, fileToMove) =>
      val originalPos = currentBlocks.indexWhere(_.id == fileToMove.id)

      {
        val index = currentBlocks.indexWhere(block => block.isEmpty() && block.length >= fileToMove.length)
        Option.when(index != -1)(index)
      } match {
        case None => currentBlocks // No space found skip
        case Some(emptySpaceIndex) =>
          if (emptySpaceIndex < originalPos) {
            val emptyBlock = currentBlocks(emptySpaceIndex)

            val afterRemoval = currentBlocks.updated(originalPos, FileBlock(-1, fileToMove.length))

            val withInsertedFile = if (emptyBlock.length == fileToMove.length) {
              afterRemoval.updated(emptySpaceIndex, fileToMove)
            } else {
              val remainingEmpty = FileBlock(-1, emptyBlock.length - fileToMove.length)
              afterRemoval.patch(emptySpaceIndex, Seq(fileToMove, remainingEmpty), 1)
            }

            // merge empty blocks
            withInsertedFile.foldLeft(IndexedSeq.empty[FileBlock]) { (acc, current) =>
              if (current.isEmpty() && acc.nonEmpty && acc.last.isEmpty()) {
                acc.init :+ FileBlock(-1, acc.last.length + current.length)
              } else {
                acc :+ current
              }
            }
          } else {
            currentBlocks
          }
      }
    }
  }

  def plotSimple(files: IndexedSeq[FileBlock]): String = {
    // only works for single digit id
    assert(files.map(_.id).max < 10)
    files.map { f =>
      if (f.id > -1) {
        f.id.toString * f.length
      } else {
        "." * f.length
      }
    }.mkString
  }
}

case class FileBlock(id: Int, length: Int) {

  override def toString: String = s"${id}:${length}"

  def isEmpty(): Boolean = id == -1
}

case class DiskMap(files: IndexedSeq[FileBlock], empties: IndexedSeq[Int]) {

  def valid(): Boolean = {
    files.filter(_.length < 0).map(fb => s"${fb.id} length ${fb.length}").isEmpty
  }

  def dump: String = {
    val fs = files.map { f =>
      s" ${f.id}:${f.length}\n"
    }.mkString
    val es = empties.mkString("\n  ")
    s"files>\n${fs}\nempties>\n  ${es}"
  }

  /** Part 1 only */
  def seq: List[Int] = {
    val blurt: Seq[List[Int]] = files.map(file => {
      val fileSeq = List.fill(file.length)(file.id)
      assert(fileSeq.length <= file.length)
      val emptySeq = Option.when(
        empties.indices.contains(file.id))(List.fill(empties(file.id))(-1)
      ).getOrElse(List.empty)
      fileSeq.concat(emptySeq)
    })
    blurt.flatten.toList
  }
}


