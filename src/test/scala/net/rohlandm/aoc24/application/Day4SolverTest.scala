package net.rohlandm.aoc24.application

import org.scalatest.funsuite.AnyFunSuiteLike

/**
 * Test cases for [[Day4Solver]]
 */
class Day4SolverTest extends AnyFunSuiteLike:

  private val testInput =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin.split("\n").toList
  

  test("solvePart1 returns result"):
    assert(Day4Solver().solvePart1(testInput) === Some(18))

  test("solvePart2 returns result"):
    assert(Day4Solver().solvePart2(testInput) === None)
