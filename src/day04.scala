package aoc
package day04

val dayNumber = "04"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

def parseRollsSet(input: String) =
  val grid = input.split("\n")
  (for
    y <- 0 until grid.length
    x <- 0 until grid.head.length
    if grid(y)(x) == '@'
  yield (x, y)).toSet

val offsets =
  for
    ox <- -1 to 1
    oy <- -1 to 1
    if !(ox == 0 && oy == 0)
  yield (ox, oy)

def findRemovable(rollsSet: Set[(Int, Int)]): Set[(Int, Int)] =
  rollsSet
    .filter((x, y) =>
      offsets
        .count((ox, oy) => rollsSet.contains((x + ox, y + oy))) < 4
    )

def part1(input: String): String =
  findRemovable(parseRollsSet(input)).size.toString

def part2(input: String): String =
  val rolls = parseRollsSet(input)

  Iterator
    .iterate((rolls = rolls, removedCount = 0))((rolls, _) =>
      val removed = findRemovable(rolls)
      rolls.diff(removed) -> removed.size
    )
    .drop(1)
    .takeWhile(_.removedCount != 0)
    .map(_.removedCount)
    .sum
    .toString
