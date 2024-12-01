package net.rohlandm.aoc24
package application

import domain.DaySolver

import scala.math.abs

class Day1Solver extends DaySolver(1):
  override def solvePart1(input: List[String]): Option[Long] =
    val unzipped = input.map(parse).unzip
    Some(unzipped._1.sorted.zip(unzipped._2.sorted).map((a, b) => abs(a - b)).sum)

  override def solvePart2(input: List[String]): Option[Long] =
    val unzipped = input.map(parse).unzip
    val lookup = unzipped._2.groupMapReduce(identity)(_ => 1)((a, b) => a + b)
    Some(unzipped._1.fold(0)((a, b) => a + (b * lookup.getOrElse(b, 0))))

  private val parse = (string: String) => string.split(" {3}").toList map (_.toInt) match
    case List(a, b) => (a, b)
    case _ => throw IllegalArgumentException()
      
