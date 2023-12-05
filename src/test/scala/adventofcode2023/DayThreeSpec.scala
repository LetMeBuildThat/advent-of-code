package adventofcode2023

import org.scalatest.funsuite.AsyncFunSuite
import cats.effect.testing.scalatest.AsyncIOSpec
import org.scalatest.matchers.should.Matchers
import DayThree._
import DayThree.Row._
import scala.collection.immutable.TreeMap

class DayThreeSpec extends AsyncFunSuite with AsyncIOSpec with Matchers {
  test("indexChars should index a String successfully") {
    val input = "467..114.."
    val input2 = ""

    val expected = Map(
      0 -> '4',
      1 -> '6',
      2 -> '7',
      3 -> '.',
      4 -> '.',
      5 -> '1',
      6 -> '1',
      7 -> '4',
      8 -> '.',
      9 -> '.'
    )

    indexChars(input) shouldBe expected
    indexChars(input2) shouldBe Map.empty
  }

  test("filterDigits should return two maps of digits and non-digit chars") {
    val input = Map(
      0 -> '4',
      1 -> '6',
      2 -> '7',
      3 -> '.',
      4 -> '.',
      5 -> '1',
      6 -> '1',
      7 -> '4',
      8 -> '.',
      9 -> '.'
    )

    val digits = Map(
      0 -> '4',
      1 -> '6',
      2 -> '7',
      5 -> '1',
      6 -> '1',
      7 -> '4'
    )

    val nonDigits = Map(
      3 -> '.',
      4 -> '.',
      8 -> '.',
      9 -> '.'
    )

    digitsAndNonDigits(input) shouldBe (digits, nonDigits)
    digitsAndNonDigits(Map.empty) shouldBe (Map.empty, Map.empty)
  }

  test(
    "filterSymbols should return only the index of chars which are symbols"
  ) {
    val input = Map(
      0 -> '*',
      3 -> '.',
      4 -> '.',
      6 -> '$',
      8 -> '.',
      9 -> '@'
    )

    val expected = Set(0, 6, 9)

    filterSymbols(input) shouldBe expected
  }

  test("groupIndexes should group consecutive index numbers") {
    val input = List(0, 1, 2, 5, 6, 7, 9)

    val expected = List(List(0, 1, 2), List(5, 6, 7), List(9))

    groupIndexes(input, List.empty, List.empty) shouldBe expected
  }

  test("groupIndexedDigits should return a number and its indexes") {
    val input = Map(
      0 -> '4',
      1 -> '6',
      2 -> '7',
      5 -> '1',
      6 -> '1',
      7 -> '4'
    )

    val expected = List(
      IndexedNumber(467, Set(0, 1, 2)),
      IndexedNumber(114, Set(5, 6, 7))
    )

    groupIndexedDigits(input) shouldBe expected
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
    val input4 = List(
      "..........573..525..&....958.777...............*..........*...........883....=....%.........158.....530..839....803............254*688......",
      "..................*..560.............&......592.........159......743.......209.....735......*.......................237.....................",
      "................15..................54.665........858..............*....93...................438.239*825.......284.....*.....565*684........",
      "......732..............574.690..........@.....471....*....@.......157....*............692.....................*........495..................",
      "...$.*.....320..9..........@......*576..........*........196..........738.................47......%955...80...983..505...............249....",
      ".468.541.........%.............134...............974..............=................424........533..........*..........*...727..............."
    )

    DayThree.part1(input) shouldBe 4361
    DayThree.part1(input2) shouldBe 1385
    DayThree.part1(input3) shouldBe 6944
    DayThree.part1(input4) shouldBe 18284
  }
}
