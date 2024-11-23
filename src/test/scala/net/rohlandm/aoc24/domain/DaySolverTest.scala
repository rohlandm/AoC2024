package net.rohlandm.aoc24
package domain

import org.scalatest.funsuite.AnyFunSuite

/**
 * Test cases for [[DaySolver]]
 */
class DaySolverTest extends AnyFunSuite:
  test("NotImplemented returns empty for solvePart1"):
    assert(NotImplemented(1).solvePart1() === None)
  
  test("NotImplemented returns empty for solvePart2"):
    assert(NotImplemented(1).solvePart2() === None)