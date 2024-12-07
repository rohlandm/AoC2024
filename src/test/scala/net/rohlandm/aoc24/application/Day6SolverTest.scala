package net.rohlandm.aoc24.application

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test cases for [[Day6Solver]]
 */
class Day6SolverTest extends AnyFunSuite:
  private val testInput =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...
      |""".stripMargin.split("\n").toList


  test("solvePart1 returns result"):
    assert(Day6Solver().solvePart1(testInput) === Some(41))

  test("solvePart2 returns result"):
    assert(Day6Solver().solvePart2(testInput) === None)

