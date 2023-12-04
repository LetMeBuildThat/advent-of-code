package adventofcode2023

import scala.collection.immutable.ListMap
import scala.io.Source

object DayThree extends App {

  case class Row(numbersIndexes: Map[Int, List[Int]], symbolIndexes: List[Int])

  object Row {
    def getEngineSchematicNumbers(rows: List[Row]): List[Int] = {
      def helper(rows: List[Row], n: Int, acc: List[Int]): List[Int] = {
        if (n > rows.length - 1)
          acc
        else {
          val engineNumbers: List[Int] =
            if (n == 0) {
              val currentRow = rows(n)
              val nextRowSymbols = rows(n + 1).symbolIndexes

              currentRow.numbersIndexes
                .filter { case (number, indexes) =>
                  val (leftAdjacent, rightAdjacent) =
                    getAdjacentIndexes(indexes)
                  // Check if there is a symbol left or right of the current number
                  currentRow.symbolIndexes.exists(i =>
                    i == leftAdjacent || i == rightAdjacent
                  ) match {
                    // Check to see if there is a symbol on the next row below or diagonally
                    case false =>
                      nextRowSymbols.exists(i =>
                        indexes.contains(
                          i
                        ) || i == leftAdjacent || i == rightAdjacent
                      )
                    case true => true
                  }
                }
                .keySet
                .toList
            } else if (n == rows.length - 1) {
              val currentRow = rows(n)
              val previousRowSymbols = rows(n - 1).symbolIndexes
              currentRow.numbersIndexes
                .filter { case (number, indexes) =>
                  val (leftAdjacent, rightAdjacent) =
                    getAdjacentIndexes(indexes)
                  // Check if there is a symbol left or right of the current number
                  currentRow.symbolIndexes.exists(i =>
                    i == leftAdjacent || i == rightAdjacent
                  ) match {
                    // Check to see if there is a symbol on the previous row above or diagonally
                    case false =>
                      previousRowSymbols.exists(i =>
                        indexes.contains(
                          i
                        ) || i == leftAdjacent || i == rightAdjacent
                      )
                    case true => true
                  }
                }
                .keySet
                .toList
            } else {
              val currentRow = rows(n)
              val previousRowSymbols = rows(n - 1).symbolIndexes
              val nextRowSymbols = rows(n + 1).symbolIndexes
              currentRow.numbersIndexes
                .filter { case (number, indexes) =>
                  val (leftAdjacent, rightAdjacent) =
                    getAdjacentIndexes(indexes)
                  // Check if there is a symbol left or right of the current number
                  currentRow.symbolIndexes.exists(i =>
                    i == leftAdjacent || i == rightAdjacent
                  ) match {
                    // Check to see if there is a symbol on either row above, below or diagonally
                    case false =>
                      val bothRowSymbols = previousRowSymbols ++ nextRowSymbols
                      bothRowSymbols.exists(i =>
                        indexes.contains(
                          i
                        ) || i == leftAdjacent || i == rightAdjacent
                      )
                    case true => true
                  }
                }
                .keySet
                .toList
            }
          helper(rows, n + 1, acc ++ engineNumbers)
        }
      }

      def getAdjacentIndexes(indexes: List[Int]): (Int, Int) =
        (indexes.head - 1, indexes.last + 1)

      helper(rows, 0, List.empty)
    }
  }

  def part1(input: List[String]): Int = {
    val rows = input
      .map { str =>
        val indexed: Map[Int, Char] =
          ListMap.from(str.zipWithIndex.map(_.swap).toSeq.sortBy(_._1))
        val digitsWithIndex: Map[Int, Char] = indexed.filter(p => p._2.isDigit)
        val numbersAndTheirIndexes = groupNumbersWithIndexes(digitsWithIndex)
        val indexesOfSymbols: List[Int] =
          indexed
            .filter(t => !t._2.isLetterOrDigit && t._2 != '.')
            .keySet
            .toList
        Row(numbersAndTheirIndexes, indexesOfSymbols)
      }
    Row.getEngineSchematicNumbers(rows).sum
  }

  def groupNumbersWithIndexes(digitMap: Map[Int, Char]): Map[Int, List[Int]] = {
    def helper(
        digitMap: Map[Int, Char],
        buff: Map[Int, Char],
        acc: Map[Int, List[Int]]
    ): Map[Int, List[Int]] = {
      if (digitMap.isEmpty)
        if (buff.nonEmpty)
          acc ++ Map(buff.values.mkString.toInt -> buff.keySet.toList)
        else
          acc
      else {
        val (index, digitChar) = digitMap.head
        buff.lastOption match {
          case None => helper(digitMap.tail, Map(index -> digitChar), acc)
          case Some((buffIndex, buffDigit)) => {
            if (index == buffIndex + 1)
              helper(digitMap.tail, buff ++ Map(index -> digitChar), acc)
            else {
              val number = buff.values.mkString.toInt
              val indexes = buff.keySet
              helper(
                digitMap.tail,
                Map(index -> digitChar),
                acc ++ Map(number -> indexes.toList)
              )
            }
          }
        }
      }
    }
    helper(digitMap, Map.empty[Int, Char], Map.empty[Int, List[Int]])
  }

  val input = Source
    .fromFile(
      getClass.getResource("/adventofcode2023/day-three.txt").getPath
    )
    .getLines()
    .toList

  println(part1(input))

}
