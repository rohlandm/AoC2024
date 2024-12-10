package net.rohlandm.aoc24

import net.rohlandm.aoc24.adapters.FileAndDownloadInputRepositoryImpl
import net.rohlandm.aoc24.application.*
import net.rohlandm.aoc24.domain.NotImplemented


@main
def main(day: String): Unit =
  val dayNumber = try day.toInt catch
    case _ =>
      printUsageWithReason("Can't parse argument")
      return

  dayNumber match
    case 1 => Day1Solver().print(getInput(dayNumber))
    case 2 => Day2Solver().print(getInput(dayNumber))
    case 3 => Day3Solver().print(getInput(dayNumber))
    case 4 => Day4Solver().print(getInput(dayNumber))
    case 5 => Day5Solver().print(getInput(dayNumber))
    case 6 => Day6Solver().print(getInput(dayNumber))
    case 7 => Day7Solver().print(getInput(dayNumber))
    case x if 1 to 25 contains x => NotImplemented(x).print(List.empty)
    case _ => printUsageWithReason("The Advent of Code has only days 1 to 25")

def printUsageWithReason(reason: String): Unit =
  print(
    s"""$reason
       |$usage
       |""".stripMargin)

val getInput: Int => List[String] = day => FileAndDownloadInputRepositoryImpl().getInput(day)

val usage =
  """
    |Usage: add the number of the day to be solved as first command line argument
    | e.g. scala run AoCSolver.scala -- 1
    |""".stripMargin