package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.application.ReportSafety.{SAFE, UNSAFE}
import net.rohlandm.aoc24.domain.DaySolver

class Day2Solver extends DaySolver(2):
  override def solvePart1(input: List[String]): Option[Long] =
    val result = input map parse groupBy isSafe
    result.get(SAFE) map (_.size.toLong) orElse Some(0)

  private val isSafe = (report: List[Int]) => report match
    case x if x.sliding(2).forall(step => 1 to 3 contains (step.head - step.last)) => SAFE
    case x if x.sliding(2).forall(step => 1 to 3 contains (step.last - step.head)) => SAFE
    case _ => UNSAFE

  private val parse = (string: String) => string.split(" ").toList map (_.toInt)

enum ReportSafety:
  case SAFE, UNSAFE