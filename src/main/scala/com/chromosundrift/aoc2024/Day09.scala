package com.chromosundrift.aoc2024

import scala.io.Source
import scala.util.matching.Regex

//noinspection ScalaWeakerAccess
object Day09 {
  private val degragRx: Regex = """^(\d*)\.(.*)(\d)(\.*)$""".r
  private val EMPTY_ID = -1

  def getInput(file: String): String = Source.fromResource(file).mkString

  def main(args: Array[String]): Unit = {
    val input = getInput("day09_input2.txt").trim
    require(input.matches("[\\d\\.]+"), "invalid input")
    println(s"part 1 ${part1(input)}")
    println(s"part 2 ${part2(input)}")
  }

  def part1(input: String) = checksumPart1(defragPart1(parsePart1(input).seq))

  def part2(input: String) = checksumPart2(defragPart2(parsePart2(input)))

  def checksumPart2(files: Seq[FileBlock]): Long = {
    files.flatMap(f => List.fill(f.length)(f.id.toLong))
      .zipWithIndex
      .map((id, index) => if (id >= 0) id * index else 0)
      .sum
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

  def checksumPart1(seq: List[Int]): Long = seq.filter(_ >= 0).zipWithIndex.map(_ * _.toLong).sum

  /**
   * List of variable size fileblocks which may be empty
   */
  def parsePart2(input: String): IndexedSeq[FileBlock] =
    input.map(_.asDigit).zipWithIndex.map { case (len, index) =>
      FileBlock(if (index % 2 == 0) index / 2 else EMPTY_ID, len)
    }

  def defragPart1(blocks: List[Int]): List[Int] = {
    // two indices counting from each direction to swap
    var left = 0
    var right = blocks.length - 1
    val plotArray = blocks.toArray
    while (left < right) {
      // only move 1 at a time
      if (plotArray(left) != EMPTY_ID) {
        left += 1
      } else if (plotArray(right) == EMPTY_ID) {
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
    val fileBlocks = files.filterNot(_.isEmpty()).sortBy(_.id)(Ordering[Int].reverse)

    fileBlocks.foldLeft(files) { (currentBlocks, fileToMove) =>
      val originalPos = currentBlocks.indexWhere(_.id == fileToMove.id)
      currentBlocks.indexWhere(block => block.isEmpty() && block.length >= fileToMove.length) match {
        case EMPTY_ID => currentBlocks // No space found, skip
        case emptyIdx if emptyIdx < originalPos =>
          val emptyBlock = currentBlocks(emptyIdx)
          val updatedBlocks = currentBlocks.updated(originalPos, FileBlock(EMPTY_ID, fileToMove.length))

          val withInsertedFile =
            if (emptyBlock.length == fileToMove.length) {
              updatedBlocks.updated(emptyIdx, fileToMove)
            } else {
              val leftovers = FileBlock(EMPTY_ID, emptyBlock.length - fileToMove.length)
              updatedBlocks.patch(emptyIdx, Seq(fileToMove, leftovers), 1)
            }

          // Merge adjacent empty blocks
          withInsertedFile.foldLeft(IndexedSeq.empty[FileBlock]) { (acc, current) =>
            acc.lastOption match {
              case Some(last) if last.isEmpty() && current.isEmpty() =>
                acc.init :+ FileBlock(EMPTY_ID, last.length + current.length)
              case _ => acc :+ current
            }
          }
        case _ => currentBlocks
      }
    }
  }

  /**
   * Only works on examples where file id is single digit
   */
  def plotSimple(files: IndexedSeq[FileBlock]): String = {
    assert(files.forall(_.id < 10))
    files.map(f => (if (f.id > EMPTY_ID) f.id.toString else ".") * f.length).mkString
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


