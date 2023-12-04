package adventofcode2023

import org.scalatest.funsuite.AsyncFunSuite
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import DayTwo._
import scala.collection.immutable.TreeMap

class DayThreeSpec extends AsyncFunSuite with AsyncIOSpec with Matchers {
  test(
    "DayThree.groupNumbersWithIndexes should key by number and group its indexes"
  ) {
    val input: Map[Int, Char] = TreeMap(
      0 -> '1',
      1 -> '2',
      2 -> '3',
      4 -> '9',
      6 -> '2',
      7 -> '4',
      8 -> '6'
    )

    val expected: Map[Int, List[Int]] = Map(
      123 -> List(0, 1, 2),
      9 -> List(4),
      246 -> List(6, 7, 8)
    )

    DayThree.groupNumbersWithIndexes(input) shouldBe expected
  }
  test("DayThree.part1 calculates the test input correctly") {
    val input = List(
      "467..114..",
      "...*......",
      "..35..633.",
      "......#...",
      "617*......",
      ".....+.58.",
      "..592.....",
      "......755.",
      "...$.*....",
      ".664.598.."
    )

    val input2 = List(
      "4.5.6.*789",
      "-.........",
      "----------",
      "123..456..",
      "&12..%1..."
    )

    val input3 = List(
      ".......*..150............355...................371...430.....155.107...............322....+..137.303......424..772......33..................",
      "778...871.......*769.290*..........&..............................*................./.........................*...................743.153...",
      ".............140............938..763..........3...............443..510....868...579.....577/....264............572.........#........*...$..."
    )

    DayThree.part1(input) shouldBe 4361
    DayThree.part1(input2) shouldBe 1385
    DayThree.part1(input3) shouldBe 6944
  }
}
