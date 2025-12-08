package aoc
package day07

val dayNumber = "07"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

def part1(input: String): Long =
  val lines = input.split('\n')

  def projectBeam(acc: (beams: Set[Int], count: Int), line: String): (Set[Int], Int) =
    (
      acc.beams.flatMap { beam =>
        if line(beam) == '^' then List(beam - 1, beam + 1)
        else List(beam)
      },
      acc.count + acc.beams.count(line(_) == '^')
    )

  val (_, splitCount) = lines.tail.foldLeft(Set(lines.head.indexOf('S')) -> 0)(projectBeam)
  splitCount

def part2(input: String): Long =
  val lines = input.split('\n').toList

  final case class Timeline(beam: Int, paths: Long)

  def projectBeam(timelines: List[Timeline], line: String): List[Timeline] =
    timelines
      .flatMap(t =>
        if line(t.beam) == '^' then
          List(
            Timeline(t.beam - 1, t.paths),
            Timeline(t.beam + 1, t.paths)
          )
        else List(t)
      ).groupMapReduce(_.beam)(_.paths)(_ + _)
      .map(Timeline(_, _))
      .toList

  val timelines = lines.tail.foldLeft(List(Timeline(lines.head.indexOf('S'), 1)))(projectBeam)
  timelines.map(_.paths).sum
