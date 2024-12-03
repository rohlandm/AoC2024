package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.application.Execution.ENABLED
import net.rohlandm.aoc24.domain.DaySolver

class Day3Solver extends DaySolver(3):

  private val mulPattern = """mul\((\d+),(\d+)\)""".r
  private val conditionalMulPattern = """mul\((\d+),(\d+)\)|do\(\)|don't\(\)""".r

  override def solvePart1(input: List[String]): Option[Long] =
    val instructions = input.flatMap(mulPattern.findAllIn(_))
    Some(instructions.map(executeInstruction).sum)

  override def solvePart2(input: List[String]): Option[Long] =
    val instructions = input.flatMap(conditionalMulPattern.findAllIn(_))
    var execution = ENABLED
    var acc: Long = 0

    instructions.foreach(instruction => {
      val result = executeConditionally.apply(instruction, execution, acc)
      acc = result._1
      execution = result._2
    })

    Some(acc)

  private val executeInstruction = (instruction: String) =>
    val split = instruction.split("[(),]").toList
    var execution = Execution.ENABLED
    split.apply(1).toInt * split.apply(2).toInt

  private val executeConditionally = (instruction: String, execution: Execution, acc: Long) =>
    instruction match
      case "do()" => (acc, Execution.ENABLED)
      case "don't()" => (acc, Execution.DISABLED)
      case mul if execution == ENABLED => (acc + executeInstruction.apply(mul), execution)
      case _ => (acc, execution)

enum Execution:
  case ENABLED, DISABLED
