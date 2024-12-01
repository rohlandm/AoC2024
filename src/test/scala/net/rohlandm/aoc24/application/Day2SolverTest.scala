package net.rohlandm.aoc24.application

import org.scalatest.funsuite.AnyFunSuiteLike

class Day2SolverTest extends AnyFunSuiteLike:

  test("solvePart1 returns result"):
    assert(Day2Solver().solvePart1(List.empty) === None)

  test("solvePart2 returns result"):
    assert(Day2Solver().solvePart2(List.empty) === None)
