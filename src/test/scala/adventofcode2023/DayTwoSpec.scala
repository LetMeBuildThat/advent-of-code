package adventofcode2023

import org.scalatest.funsuite.AsyncFunSuite
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import DayTwo._

class DayTwoSpec extends AsyncFunSuite with AsyncIOSpec with Matchers {

  test("DayTwo.part1 sums the ids of valid games") {
    val input = List(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )

    val rule = RedGreenBlue(12, 13, 14)

    DayTwo.part1(input, rule) shouldBe 8
  }
  test("DayTwo.part1 returns 0 if no games are provided") {
    val input = List.empty[String]

    val rule = RedGreenBlue(12, 13, 14)

    DayTwo.part1(input, rule) shouldBe 0
  }
  test("DayTwo.part2 sums the power of minimum cubes correctly") {
    val input = List(
      "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
      "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
      "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
      "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
      "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
    )

    DayTwo.part2(input) shouldBe 2286
  }
}
