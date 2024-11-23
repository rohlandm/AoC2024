package net.rohlandm.aoc24
package application

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test cases for [[Day1Solver]]
 */
class Day1SolverTest extends AnyFunSuite:
  test("solvePart1 returns Empty"):
    assert(Day1Solver().solvePart1() === Option.empty)

  test("solvePart2 returns Empty"):
    assert(Day1Solver().solvePart2() === Option.empty)
