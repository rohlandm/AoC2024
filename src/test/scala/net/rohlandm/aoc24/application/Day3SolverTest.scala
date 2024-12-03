package net.rohlandm.aoc24.application

import org.scalatest.funsuite.AnyFunSuiteLike

/**
 * Test cases for [[Day3Solver]]
 */
class Day3SolverTest extends AnyFunSuiteLike:

  private val testInput1 =
    """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))""".stripMargin.split("\n").toList

  private val testInput2 =
    """xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))""".stripMargin.split("\n").toList

  test("solvePart1 returns result"):
    assert(Day3Solver().solvePart1(testInput1) === Some(161))

  test("solvePart2 returns result"):
    assert(Day3Solver().solvePart2(testInput2) === Some(48))
