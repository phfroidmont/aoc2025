package aoc
package day02

import scala.annotation.tailrec

val dayNumber = "02"

@main def part1: Unit =
  println(part1(loadInput(dayNumber)))

@main def part2: Unit =
  println(part2(loadInput(dayNumber)))

def part1(input: String): String =

  def isValid(id: String): Boolean =
    if id.length % 2 == 0 then
      val (left, right) = id.splitAt(id.length / 2)
      left != right
    else true

  input
    .split(",")
    .map { case s"$from-$to" => from.toLong to to.toLong }
    .map(_.filterNot(id => isValid(id.toString)).sum)
    .sum
    .toString

def part2(input: String): String =

  def isValid(id: Long): Boolean =
    @tailrec
    def rec(id: String, groupsCount: Int): Boolean =
      if groupsCount < 1 then true
      else if id.length % groupsCount == 0 then
        id.grouped(groupsCount).distinct.length > 1 && rec(id, groupsCount - 1)
      else rec(id, groupsCount - 1)

    rec(id.toString, id.toString.length - 1)

  input
    .split(",")
    .map { case s"$from-$to" => from.toLong to to.toLong }
    .map(_.filterNot(isValid).sum)
    .sum
    .toString
