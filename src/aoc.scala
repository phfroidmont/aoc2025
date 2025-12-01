package aoc

import scala.io.Source
import scala.util.Using

def loadInput(dayNumber: String) =
  Using.resource(Source.fromFile(s"input/day$dayNumber"))(_.mkString.trim)
