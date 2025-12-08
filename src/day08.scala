package aoc
package day08

import scala.math.pow

val dayNumber = "08"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

case class Box(x: Long, y: Long, z: Long):
  def distanceSquared(other: Box): Double =
    pow(x - other.x, 2) + pow(y - other.y, 2) + pow(z - other.z, 2)

type Circuit = Set[Box]
extension (circuit: Circuit)
  def distances = circuit.toList
    .combinations(2)
    .collect {
      case List(b1, b2) if b1 != b2 =>
        (b1, b2, b1.distanceSquared(b2))
    }.toList.sortBy(_._3)

extension (circuits: Vector[Circuit])
  def connect(b1: Box, b2: Box) =
    val idx1 = circuits.indexWhere(_.contains(b1))
    val idx2 = circuits.indexWhere(_.contains(b2))
    val c1   = circuits(idx1)
    val c2   = circuits(idx2)
    circuits.updated(idx2, Set.empty).updated(idx1, c1 ++ c2).filter(_.nonEmpty)

def part1(input: String): Long =
  val boxes = input.split('\n').map { case s"$x,$y,$z" => Box(x.toInt, y.toInt, z.toInt) }.toSet

  val circuits =
    boxes.distances.take(1000).foldLeft(boxes.toVector.map(Set(_))) {
      case (circuits, (b1, b2, _)) =>
        circuits.connect(b1, b2)
    }

  circuits.toList.map(_.size).sorted.takeRight(3).reduce(_ * _)

def part2(input: String): Long =
  val boxes = input.split('\n').map { case s"$x,$y,$z" => Box(x.toInt, y.toInt, z.toInt) }.toSet

  var lastDistance = Option.empty[(Box, Box)]

  boxes.distances
    .to(LazyList)
    .scanLeft(boxes.toVector.map(Set(_))) { case (circuits, (b1, b2, _)) =>
      lastDistance = Some(b1, b2)
      circuits.connect(b1, b2)
    }
    .dropWhile(_.size > 1).head

  lastDistance.map((b1, b2) => b1.x * b2.x).get
