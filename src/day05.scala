package aoc
package day05

val dayNumber = "05"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

def part1(input: String): String =
  val Array(rangesInput, ingredientsInput) = input.split("\n\n")
  val ranges                               =
    rangesInput.split('\n').map { case s"$from-$to" => (from = from.toLong, to = to.toLong) }
  ingredientsInput
    .split('\n').map(_.toLong)
    .count(i => ranges.exists(r => i >= r.from && i <= r.to)).toString

def part2(input: String): String =

  case class Range(from: Long, to: Long):
    def contains(i: Long): Boolean      = i >= from && i <= to
    def contains(other: Range): Boolean = contains(other.from) && contains(other.to)
    def size                            = Math.max(0, (to - from) + 1)
    def diff(other: Range): List[Range] =
      if contains(other) then List(Range(from, other.from - 1), Range(other.to + 1, to))
      else if other.contains(this) then List()
      else if contains(other.from) then List(Range(from, other.from - 1))
      else if contains(other.to) then List(Range(other.to + 1, to))
      else List(this)

  case class MultiRange(ranges: List[Range] = List.empty):
    def size                          = ranges.map(_.size).sum
    def add(range: Range): MultiRange =
      val remainingRanges = ranges.foldLeft(List(range))((acc, r) => acc.flatMap(_.diff(r)))
      MultiRange(remainingRanges ++ ranges)

  input
    .split("\n\n").head
    .split('\n')
    .map { case s"$from-$to" => Range(from.toLong, to.toLong) }
    .foldLeft(MultiRange())(_.add(_)).size.toString
