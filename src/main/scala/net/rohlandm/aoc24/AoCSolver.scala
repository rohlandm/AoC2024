package net.rohlandm.aoc24

import application.Day1Solver
import domain.NotImplemented


@main
def main(day: String): Unit =
  val dayNumber = try {
    day.toInt
  } catch
    case _ =>
      printUsageWithReason("Can't parse argument")
      return

  dayNumber match
    case 1 => Day1Solver().print()
    case x if x <= 25 => NotImplemented(x).print()
    case x if x > 25 => printUsageWithReason("The Advent of Code has only 25 days")

def printUsageWithReason(reason: String): Unit =
  print(
    s"""$reason
       |$usage
       |""".stripMargin)

val usage =
  """
    |Usage: add the number of the day to be solved as first command line argument
    | e.g. scala run AoCSolver.scala -- 1
    |""".stripMargin