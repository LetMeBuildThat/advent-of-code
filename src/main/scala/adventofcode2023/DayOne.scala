package adventofcode2023

import fs2.Stream
import cats.effect.IO
import cats.effect.implicits._
import scala.collection.immutable
import cats.effect.IOApp
import fs2.io.file.{Files, Path}

object DayOne {

  object PartOne {
    def getNumericalDigits(str: String): List[Int] =
      str.toList.collect {
        case c if c.isDigit => c.asDigit
      }
  }

  object PartTwo {
    private def stringToDigit(str: String): Option[Int] =
      str match {
        case one if one.contains("one")       => Some(1)
        case two if two.contains("two")       => Some(2)
        case three if three.contains("three") => Some(3)
        case four if four.contains("four")    => Some(4)
        case five if five.contains("five")    => Some(5)
        case six if six.contains("six")       => Some(6)
        case seven if seven.contains("seven") => Some(7)
        case eight if eight.contains("eight") => Some(8)
        case nine if nine.contains("nine")    => Some(9)
        case _                                => None
      }

    def getVerboseAndNumericalDigits(str: String): List[Int] = {
      def recurseStringForDigits(
          str: String,
          acc: String,
          digits: List[Int]
      ): List[Int] = {
        if (str.isEmpty) {
          stringToDigit(acc).fold(digits)(i => digits :+ i)
        } else if (str.head.isDigit) {
          recurseStringForDigits(str.tail, "", digits :+ str.head.asDigit)
        } else {
          val newAcc = acc :+ str.head
          stringToDigit(newAcc).fold(
            recurseStringForDigits(str.tail, newAcc, digits)
          )(i => recurseStringForDigits(str, "", digits :+ i))
        }
      }
      recurseStringForDigits(str, "", List.empty[Int])
    }
  }

  def calibrationSum(s: Stream[IO, String], f: String => List[Int]): IO[Int] =
    s.map { str =>
      val n = f(str)
      (n.headOption, n.lastOption) match {
        case (Some(h), Some(l)) => (h * 10) + l
        case (Some(h), None)    => (h * 10) + h
        case _                  => 0
      }
    }.compile
      .toList
      .map(_.sum)
}
