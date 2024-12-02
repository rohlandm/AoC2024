package net.rohlandm.aoc24.application

import org.scalatest.funsuite.AnyFunSuiteLike

/**
 * Test cases for [[Day2Solver]]
 */
class Day2SolverTest extends AnyFunSuiteLike:

  private val testInput =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9
      |""".stripMargin.split("\n").toList
  
  test("solvePart1 returns result"):
    assert(Day2Solver().solvePart1(testInput) === Some(2))

  test("solvePart2 returns result"):
    assert(Day2Solver().solvePart2(testInput) === Some(4))
