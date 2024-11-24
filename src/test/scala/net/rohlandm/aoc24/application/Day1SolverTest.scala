package net.rohlandm.aoc24
package application

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test cases for [[Day1Solver]]
 */
class Day1SolverTest extends AnyFunSuite:
  test("solvePart1 returns result"):
    assert(Day1Solver().solvePart1(List()) === Some(1))

  test("solvePart2 returns result"):
    assert(Day1Solver().solvePart2(List()) === Some(2))
