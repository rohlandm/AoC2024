package net.rohlandm.aoc24
package application

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test cases for [[Day1Solver]]
 */
class Day1SolverTest extends AnyFunSuite:

  private val testInput =
    """3   4
      |4   3
      |2   5
      |1   3
      |3   9
      |3   3
      |""".stripMargin.split("\n").toList

  test("solvePart1 returns result"):
    assert(Day1Solver().solvePart1(testInput) === Some(11))

  test("solvePart2 returns result"):
    assert(Day1Solver().solvePart2(testInput) === Some(31))
