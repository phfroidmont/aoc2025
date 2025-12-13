package aoc
package day09

val dayNumber = "09"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

def part1(input: String): Long =
  input
    .split('\n').map { case s"$x,$y" => (x = x.toLong, y = y.toLong) }
    .combinations(2)
    .collect { case Array(a, b) => ((a.x - b.x + 1) * (a.y - b.y + 1)).abs }
    .max

def part2(input: String): Long =

  case class Point(x: Int, y: Int)

  case class Rectangle(xs: Range, ys: Range):
    def area: Long           = xs.size.toLong * ys.size.toLong
    def dropEdges: Rectangle = Rectangle(xs.drop(1).dropRight(1), ys.drop(1).dropRight(1))
    def intersects(other: Rectangle): Boolean =
      xs.intersects(other.xs) && ys.intersects(other.ys)

  object Rectangle:
    def between(a: Point, b: Point): Rectangle =
      val diffX = b.x - a.x
      val diffY = b.y - a.y
      Rectangle(
        a.x to b.x by (if diffX == 0 then 1 else diffX.sign),
        a.y to b.y by (if diffY == 0 then 1 else diffY.sign)
      )

  extension (r: Range)
    def intersects(other: Range): Boolean =
      r.nonEmpty && other.nonEmpty &&
        (r.contains(other.min) || other.contains(r.min))

  val points = input.split('\n').collect { case s"$x,$y" => Point(x.toInt, y.toInt) }.toList
  val edges  =
    (points.last :: points)
      .sliding(2)
      .collect { case List(a, b) => Rectangle.between(a, b) }
      .toList

  val rectangles = points
    .combinations(2)
    .collect { case List(a, b) => Rectangle.between(a, b) }
    .toList

  rectangles.collect { case r if !edges.exists(r.dropEdges.intersects) => r.area }.max

end part2
