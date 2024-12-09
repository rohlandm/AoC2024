package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.application.Operator.{ADD, MUL}
import net.rohlandm.aoc24.domain.DaySolver

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

class Day7Solver extends DaySolver(7):
  override def solvePart1(input: List[String]): Option[Long] =
    val equations = input.map(parse)
    val operations = equations.map(eq => (eq, generateOperatorPermutations(Operator.values.toSeq, eq.members.length)))
    Some(operations.filter(evaluateEquation).map((eq, _) => eq.result).sum)

  @tailrec
  private def evaluateEquation(equation: Equation, permutations: Seq[Seq[Operator]]): Boolean =
    permutations match
      case x if x.length == 1 => evaluateHead(equation = equation, operators = permutations.head)
      case _ => evaluateHead(equation = equation, operators = permutations.head) || evaluateEquation(equation = equation, permutations = permutations.tail)

  @tailrec
  private def evaluateHead(equation: Equation, operators: Seq[Operator]): Boolean =
    equation.members match
      case x if x.length == 2 => operators.head match
        case ADD => x.take(2).sum == equation.result
        case MUL => x.take(2).product == equation.result
      case x =>
        val firstResult = operators.head match
          case ADD => x.take(2).sum
          case MUL => x.take(2).product
        evaluateHead(equation = Equation(equation.result, firstResult +: x.drop(2)), operators = operators.tail)

  private def generateOperatorPermutations(options: Seq[Operator], length: Int): Seq[Seq[Operator]] =
    if length == 0 then Seq(Seq.empty)
    else
      for
        perm <- generateOperatorPermutations(options, length - 1)
        value <- options
      yield value +: perm


  private def parse(line: String): Equation =
    val split = line.split(": ").toList
    assert(split.length == 2, "First split should be of length 2")
    Equation(result = split.head.toLong, members = split.last.split(" ").toList.map(_.toLong))

case class Equation(result: Long, members: List[Long])

enum Operator:
  case ADD, MUL