package aoc
package day01

val dayNumber = "01"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

def part1(input: String): String =
  case class Dial(pos: Int = 50, zeroCount: Int = 0):
    def move(rot: Int): Dial =
      val newPos = (pos + rot) % 100
      Dial(newPos, zeroCount + (if newPos == 0 then 1 else 0))

  input
    .split('\n')
    .map {
      case s"L$rot" => -rot.toInt
      case s"R$rot" => rot.toInt
    }
    .foldLeft(Dial())(_.move(_))
    .zeroCount.toString

def part2(input: String): String =
  case class Dial(pos: Int = 50, zeroCount: Int = 0):
    def move(rot: Int): Dial =
      val newPos      = pos + rot
      val signChanged = (pos > 0 && newPos <= 0) || (pos < 0 && newPos >= 0)
      val zeroPasses  = Math.abs(newPos / 100) + (if signChanged then 1 else 0)
      Dial(newPos % 100, zeroCount + zeroPasses)

  input
    .split('\n')
    .map {
      case s"L$rot" => -rot.toInt
      case s"R$rot" => rot.toInt
    }
    .foldLeft(Dial())(_.move(_))
    .zeroCount.toString
