package adventofcode2023

import scala.io.Source
import scala.annotation.tailrec

object DayFour extends App {
  def part1(input: List[String]): Int = {
    input
      .map(Card.fromCardString(_).points)
      .sum
  }
  def part2(input: List[String]): Int = {
    val cardsIndexed = input
      .map(Card.fromCardString(_))

    countWinners(cardsIndexed)
  }

  def countWinners(cards: List[Card]): Int = {
    def helper(remaining: List[(Card, Int)], wins: Int): Int = {
      remaining match {
        case Nil => wins
        case (card, count) :: tl => {
          val (copies, process) = tl.splitAt(card.matches)
          val newCopies = copies.map { case (c, n) => (c, n + count) }
          helper(newCopies ++ process, wins + count)
        }
      }
    }
    helper(cards.map(c => c -> 1), 0)
  }

  case class Card(
      n: Int,
      matchTo: List[Int],
      matchWith: List[Int]
  ) {
    val matches: Int = matchTo.intersect(matchWith).size
    def points: Int =
      if (matches > 0) (1 to matches - 1).foldLeft(1)((acc, _) => acc * 2)
      else 0
  }

  object Card {
    def fromCardString(str: String): Card =
      str match {
        case s"Card $i: $w | $c" =>
          val matchTo = parseStringsToNumbers(w.split(" ").toList)
          val matchWith = parseStringsToNumbers(c.split(" ").toList)
          Card(i.trim.toInt, matchTo, matchWith)
      }
    def parseStringsToNumbers(l: List[String]): List[Int] =
      l.flatMap { case s"$i" =>
        i.toIntOption
      }
  }

  val input = Source
    .fromFile(
      getClass.getResource("/adventofcode2023/day-four.txt").getPath
    )
    .getLines()
    .toList

  println(part1(input)) // 18619
  println(part2(input)) // 8063216
}
