package adventofcode2023

import org.scalatest.funsuite.AsyncFunSuite
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import fs2.Stream
import cats.effect.IO
import adventofcode2023.DayOne
import fs2.io.file.Files
import fs2.io.file.Path
import DayOne.PartOne._
import DayOne.PartTwo._

class DayOneSpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  test(
    "DayOne.getNumericalDigits takes a String and returns all the numerical digits as integers"
  ) {
    val input = "a1b2"
    val input2 = "nf3rnfjr3nfiunruiev5"
    val input3 = "jufnje55rnfvkjferbhj66fvberhbfrhb22"
    getNumericalDigits(input) shouldBe List(1, 2)
    getNumericalDigits(input2) shouldBe List(3, 3, 5)
    getNumericalDigits(input3) shouldBe List(5, 5, 6, 6, 2, 2)
  }

  test(
    "DayOne.calibrationSum takes the first and last digit of each String and sums them all"
  ) {
    val input = Stream[IO, String]("a1b2", "a3b6", "ccc7cccc")

    DayOne
      .calibrationSum(input, getNumericalDigits)
      .asserting(_ shouldBe 125)
  }

  test("DayOne.calibrationSum calculates part one correctly") {
    val stream: Stream[IO, String] = Files[IO].readUtf8Lines(
      Path(getClass.getResource("/adventofcode2023/day-one.txt").getPath())
    )
    DayOne
      .calibrationSum(stream, getNumericalDigits)
      .asserting(_ shouldBe 55488)
  }
  test(
    "DayOne.getVerboseAndNumericalDigits returns all the verbose and numerical digits as integers"
  ) {
    val input = "oneb1"
    val input2 = "twofivesix"
    val input3 = "c12c"
    val input4 = "cccccccccc"
    val input5 = ""

    getVerboseAndNumericalDigits(input) shouldBe List(1, 1)
    getVerboseAndNumericalDigits(input2) shouldBe List(2, 5, 6)
    getVerboseAndNumericalDigits(input3) shouldBe List(1, 2)
    getVerboseAndNumericalDigits(input4) shouldBe List.empty[Int]
    getVerboseAndNumericalDigits(input5) shouldBe List.empty[Int]
  }
  test(
    "DayOne.calibrateSum takes the first and last digit, verbose or numerical, of each String and sums them all"
  ) {

    val input = Stream[IO, String](
      "two1nine",
      "eightwothree",
      "abcone2threexyz",
      "xtwone3four",
      "4nineeightseven2",
      "zoneight234",
      "7pqrstsixteen"
    )

    DayOne
      .calibrationSum(input, getVerboseAndNumericalDigits)
      .asserting(_ shouldBe 281)
  }
  test("DayOne.calibrationSum calculates part two correctly") {
    val stream: Stream[IO, String] = Files[IO].readUtf8Lines(
      Path(getClass.getResource("/adventofcode2023/day-one.txt").getPath())
    )
    DayOne
      .calibrationSum(stream, getVerboseAndNumericalDigits)
      .asserting(_ shouldBe 55614)
  }
}
