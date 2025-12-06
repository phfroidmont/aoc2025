package aoc
package day06

val dayNumber = "06"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

def part1(input: String): String =
  val lines   = input.split('\n').map(_.trim)
  val numbers = lines.init.map(_.split("\\s+").map(_.toLong)).transpose
  val ops     = lines.last.split("\\s+").map[(Long, Long) => Long] {
    case "*" => _ * _
    case "+" => _ + _
  }
  numbers.zip(ops).map((numbers, op) => numbers.reduce(op)).sum.toString

def part2(input: String): String =
  val columns  = input.split('\n').map(_.toArray).transpose.map(_.mkString).mkString("\n")
  val problems = columns.split("\n +\n")
  problems
    .map(p =>
      val lines   = p.split('\n')
      val numbers = lines.map(_.filter(_.isDigit)).filterNot(_.isBlank).map(_.toLong)
      val op: (Long, Long) => Long = lines.head.last match
        case '*' => _ * _
        case '+' => _ + _
      numbers.reduce(op)
    ).sum.toString
