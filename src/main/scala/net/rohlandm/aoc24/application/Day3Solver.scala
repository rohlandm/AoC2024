package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.domain.DaySolver

class Day3Solver extends DaySolver(3):
  
  private val mulPattern = """mul\((\d+),(\d+)\)""".r
  
  override def solvePart1(input: List[String]): Option[Long] =
    val instructions = input.flatMap(line => mulPattern.findAllIn(line))
    Some(instructions.map(executeInstruction).sum)

  private val executeInstruction = (string: String) => 
    val split = string.split("[(),]").toList
    split.apply(1).toInt * split.apply(2).toInt