package net.rohlandm.aoc24.application

import org.scalatest.funsuite.AnyFunSuiteLike

/**
 * Test cases for [[Day3Solver]]
 */
class Day3SolverTest extends AnyFunSuiteLike:

  private val testInput =
    """""".stripMargin.split("\n").toList

  test("solvePart1 returns result"):
    assert(Day3Solver().solvePart1(testInput) === None)

  test("solvePart2 returns result"):
    assert(Day3Solver().solvePart2(testInput) === None)
