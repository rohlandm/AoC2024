package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.application.Operator.{ADD, CONCAT, MUL}
import net.rohlandm.aoc24.domain.DaySolver

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class Day7Solver extends DaySolver(7):
  override def solvePart1(input: List[String]): Option[Long] =
    val equations = input.map(parse)
    Some(equations.filter(eq => evaluateEquation(reversedMembers = eq.members.reverse, answer = eq.result, operators = Seq(ADD, MUL))).map(_.result).sum)

  override def solvePart2(input: List[String]): Option[Long] =
    val equations = input.map(parse)
    Some(equations.filter(eq => evaluateEquation(reversedMembers = eq.members.reverse, answer = eq.result, operators = Seq(ADD, MUL, CONCAT))).map(_.result).sum)

  private def evaluateEquation(reversedMembers: Seq[Long], answer: Long, operators: Seq[Operator]): Boolean =
    reversedMembers match
      case x if x.length == 1 => x.head == answer
      case x => operators.exists(op =>
        op match
          case ADD => evaluateEquation(reversedMembers = x.tail, answer = answer - x.head, operators = operators)
          case MUL => answer % x.head == 0 && evaluateEquation(reversedMembers = x.tail,
            answer = answer / x.head,
            operators = operators)
          case CONCAT => answer.toString.endsWith(x.head.toString) && evaluateEquation(reversedMembers = x.tail,
            answer = unConcat(answer, x.head),
            operators = operators)
      )

  private def unConcat(a: Long, b: Long): Long =
    val string = a.toString.dropRight(b.toString.length)
    if string.isBlank then 0 else string.toLong


  private def parse(line: String): Equation =
    val split = line.split(": ").toList
    assert(split.length == 2, "First split should be of length 2")
    Equation(result = split.head.toLong, members = split.last.split(" ").toSeq.map(_.toLong))

case class Equation(result: Long, members: Seq[Long])

enum Operator:
  case ADD, MUL, CONCAT
