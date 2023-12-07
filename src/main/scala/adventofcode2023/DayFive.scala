package adventofcode2023

import cats.effect.ExitCode
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.implicits._
import cats.implicits._
import fs2.Chunk
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import DayFive.Domain.{Category, Seeds, Input, SeedsWithCategories}
import DayFive.Domain.Category.CategoryMap

import scala.collection.immutable

/*
  Instead of returning the whole range, get the min and max for the source/destination and the diff.
  e.g d: 52 s: 50 n: 48 -> sRange: [50, 97], dDiff: +2
      -> source: 79
      -> 79: within sRange
      -> 79 + 2 => 81
 */

object DayFive extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    val input = Files[IO]
      .readUtf8Lines(
        Path(
          getClass()
            .getResource("/adventofcode2023/day-five.txt")
            .getPath()
        )
      )
      .split(_.trim().isEmpty())

    for {
      // _ <- part1(input)
      _ <- part2(input)
    } yield ExitCode.Success
  }

  def part1(input: Stream[IO, Chunk[String]]): IO[Unit] =
    for {
      inputs <- input
        .flatMap { c =>
          Stream.chunk(flattenChunk(processChunk(c, strToLongs)))
        }
        .compile
        .toList
      swc <- IO.pure(SeedsWithCategories(inputs))
      lowestDest = swc.getDestinations.min
      res <- IO.println(lowestDest) // 457535844
    } yield res

  def part2(input: Stream[IO, Chunk[String]]): IO[Unit] =
    for {
      inputs <- input
        .flatMap { c =>
          Stream.chunk(flattenChunk(processChunk(c, strToLongRanges)))
        }
        .evalTap(IO.println)
        .compile
        .toList
      swc <- IO.pure(SeedsWithCategories(inputs))
      lowestDest <- IO(swc.getDestinations.min)
      res <- IO.println(lowestDest)
    } yield res

  def processChunk(
      c: Chunk[String],
      seedTransform: String => Set[Long]
  ): Chunk[Option[Input]] = {
    c.map { str =>
      str match {
        case s"seeds: $numbers" => Some(Seeds(seedTransform(numbers)))
        case s"$des $src $length" => {
          (des.toLongOption, src.toLongOption, length.toLongOption) match {
            case (Some(d), Some(s), Some(l)) => {
              val sMin = s
              val sMax = s + l - 1
              val dDiff = d - s
              val cm = CategoryMap(sMin, sMax, dDiff)
              Some(Category(List(cm)))
            }
            case _ => None
          }
        }
        case _ => None
      }
    }
  }

  def strToLongs(str: String): Set[Long] =
    str.split(" ").flatMap(_.toLongOption).toSet

  def strToLongRanges(str: String): Set[Long] =
    str
      .split(" ")
      .flatMap(_.toLongOption)
      .toList
      .grouped(2)
      .flatMap {
        case List(n, range) => (n to n + (range - 1))
        case _              => List.empty
      }
      .toSet

  def flattenChunk(c: Chunk[Option[Input]]): Chunk[Input] = {
    val seeds =
      c.collect { case Some(Seeds(v)) => v }.toList.flatten.toSet
    val categoryMaps = c
      .collect { case Some(Category(maps)) =>
        maps
      }
      .toList
      .flatten
    if (seeds.nonEmpty) Chunk(Seeds(seeds)) else Chunk(Category(categoryMaps))
  }

  object Domain {
    sealed trait Input

    case class Seeds(v: Set[Long]) extends Input

    case class Category(v: List[CategoryMap]) extends Input {
      def getDestination(s: Long): Long =
        v.collectFirst {
          case CategoryMap(sMin, sMax, dDiff) if (s >= sMin && s <= sMax) =>
            s + dDiff
        }.getOrElse(s)
    }
    object Category {
      case class CategoryMap(sMin: Long, sMax: Long, dDiff: Long)
    }

    case class SeedsWithCategories(s: Set[Long], c: List[Category]) {
      def getDestinations: List[Long] = {
        def helper(
            seed: Long,
            categories: List[Category],
            acc: List[Long]
        ): List[Long] = {
          categories match {
            case Nil => acc
            case h :: tl =>
              val destination = h.getDestination(seed)
              helper(destination, tl, acc :+ destination)
          }
        }
        s.toList.map { seed =>
          println(seed)
          helper(seed, c, List.empty).last
        }
      }
    }
    object SeedsWithCategories {
      def apply(input: List[Input]): SeedsWithCategories = {
        val seeds =
          input.collectFirst { case Seeds(v) => v }.getOrElse(Set.empty)
        val categories =
          input.collect { case c: Category => c }
        SeedsWithCategories(seeds, categories)
      }
    }
  }
}
