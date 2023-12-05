package adventofcode2023

import scala.collection.immutable.ListMap
import scala.io.Source
import scala.collection.immutable
import scala.util.Try

object DayThree extends App {

  case class Row(indexedNumbers: Map[Int, List[Int]], symbolIndexes: Set[Int]) {
    def possibleSymbolIndexes(number: Int): Set[Int] =
      indexedNumbers.get(number) match {
        case None => Set.empty[Int]
        case Some(indexes) => {
          val leftAdj = indexes.min - 1
          val rightAdj = indexes.max + 1
          indexes.toSet + leftAdj + rightAdj
        }
      }
    def matches(indexes: Set[Int]): Boolean =
      indexes.intersect(symbolIndexes).nonEmpty
  }

  object Row {
    def fromString(str: String): Row = {
      val indexed: Map[Int, Char] = indexChars(str)
      val (digits, nonDigits): (Map[Int, Char], Map[Int, Char]) =
        digitsAndNonDigits(indexed)
      val numbers: Map[Int, List[Int]] = groupIndexedDigits(digits)
      val symbols: Set[Int] = filterSymbols(nonDigits)
      Row(numbers, symbols)
    }

    def indexChars(str: String): Map[Int, Char] =
      str.toList.zipWithIndex.map(_.swap).toMap

    def digitsAndNonDigits(
        m: Map[Int, Char]
    ): (Map[Int, Char], Map[Int, Char]) =
      m.partition { case (i, c) => c.isDigit }

    def filterSymbols(m: Map[Int, Char]): Set[Int] =
      m.filter { case (i, c) =>
        c != '.'
      }.keySet

    def groupIndexedDigits(m: Map[Int, Char]): Map[Int, List[Int]] = {
      val sortedKeys = m.keys.toList.sorted
      val groupedKeys = groupIndexes(sortedKeys, List.empty, List.empty)

      groupedKeys.flatMap { consecKeys =>
        val digits = consecKeys.flatMap { i => m.get(i) }
        val convert = digitCharsToNumber(digits)
        val numberAndIndexes = convert.map(number => number -> consecKeys)
        numberAndIndexes
      }.toMap
    }

    def groupIndexes(
        l: List[Int],
        b: List[Int],
        acc: List[List[Int]]
    ): List[List[Int]] =
      l match {
        case Nil => acc :+ b
        case head :: _ => {
          if (b.isEmpty)
            groupIndexes(l.tail, b :+ head, acc)
          else if (head == b.last + 1)
            groupIndexes(l.tail, b :+ head, acc)
          else
            groupIndexes(l.tail, List(head), acc :+ b)
        }
      }

    def digitCharsToNumber(l: List[Char]): Option[Int] =
      l.mkString.toIntOption
  }

  def part1(input: List[String]): Int = {
    val rows = input.map(Row.fromString)
    checkWinners(rows).sum
  }

  def checkWinners(rows: List[Row]): List[Int] = {
    def helper(n: Int, acc: List[Int]): List[Int] = {
      if (n == rows.size)
        acc
      else {
        val currentRow = rows(n)
        val aboveRow = Try(rows(n - 1)).toOption
        val belowRow = Try(rows(n + 1)).toOption
        val valids = currentRow.indexedNumbers.keySet.filter { number =>
          val validIndexes = currentRow.possibleSymbolIndexes(number)
          val leftOrRight = currentRow.matches(validIndexes)
          val above = aboveRow.fold(false)(row => row.matches(validIndexes))
          val below = belowRow.fold(false)(row => row.matches(validIndexes))
          leftOrRight || above || below
        }

        helper(n + 1, acc ++ valids)
      }
    }

    helper(0, List.empty)
  }

  val input = Source
    .fromFile(
      getClass.getResource("/adventofcode2023/day-three.txt").getPath
    )
    .getLines()
    .toList

  println(part1(input))

}
