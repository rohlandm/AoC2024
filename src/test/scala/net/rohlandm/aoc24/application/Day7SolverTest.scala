package net.rohlandm.aoc24.application

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test cases for [[Day7Solver]]
 */
class Day7SolverTest extends AnyFunSuite:
  private val testInput =
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20
      |""".stripMargin.split("\n").toList


  test("solvePart1 returns result"):
    assert(Day7Solver().solvePart1(testInput) === Some(3749))

  test("solvePart2 returns result"):
    assert(Day7Solver().solvePart2(testInput) === None)

