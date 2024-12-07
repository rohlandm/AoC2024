package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.domain.DaySolver

import scala.annotation.tailrec
import scala.collection.mutable

class Day5Solver extends DaySolver(5):
  override def solvePart1(input: List[String]): Option[Long] =
    val setup = parse.apply(input)
    val validMiddles = setup._2 filter (x => evaluateUpdate(x, setup._1, List.empty)) map (x => x.apply(x.length / 2))
    Some(validMiddles.sum)

  @tailrec
  private def evaluateUpdate(update: List[Int], rules: List[PrintRule], before: List[Int]): Boolean =
    update match
      case x if update.size == 1 => rules.filter(_.after == x.head).map(_.before).intersect(before).distinct.sorted == before.sorted
      case x if !(rules.filter(_.before == x.head).map(_.after).intersect(x.tail).distinct.sorted == x.tail.sorted) => false
      case x => evaluateUpdate(x.tail, rules, before.appended(x.head))

  private val parse = (input: List[String]) => {
    val rules = mutable.ListBuffer.empty[PrintRule]
    val updates = mutable.ListBuffer.empty[List[Int]]
    input foreach {
      case x if x contains '|' => rules.addOne(PrintRule(x.split('|').apply(0).toInt, x.split('|').apply(1).toInt))
      case x if x contains ',' => updates.addOne(x.split(',').toList.map(_.toInt))
      case _ => ()
    }
    (rules.toList, updates.toList)
  }


case class PrintRule(before: Int, after: Int)
