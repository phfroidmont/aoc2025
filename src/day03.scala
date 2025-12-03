package aoc
package day03

import scala.annotation.tailrec

val dayNumber = "03"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

def part1(input: String): String =

  def solve(digits: String) =
    val highestDigitIdx = digits.dropRight(1).zipWithIndex.maxBy(_._1)._2
    s"${digits(highestDigitIdx)}${digits.drop(highestDigitIdx + 1).max}".toInt

  input
    .split('\n')
    .map(solve)
    .sum
    .toString

def part2(input: String): String =

  def solve(digits: String) =
    @tailrec
    def rec(digits: String, selected: List[Char], toSelectCount: Int): Long =
      if toSelectCount == 0 then selected.reverse.mkString.toLong
      else
        val highestDigitIdx = digits.dropRight(toSelectCount - 1).zipWithIndex.maxBy(_._1)._2
        rec(
          digits.drop(highestDigitIdx + 1),
          digits(highestDigitIdx) :: selected,
          toSelectCount - 1
        )

    rec(digits, List.empty, 12)

  input
    .split('\n')
    .map(solve)
    .sum
    .toString
