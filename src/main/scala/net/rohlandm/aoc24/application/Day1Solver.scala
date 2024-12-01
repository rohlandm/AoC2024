package net.rohlandm.aoc24
package application

import domain.DaySolver

import scala.math.abs

class Day1Solver extends DaySolver(1):
  override def solvePart1(input: List[String]): Option[Long] =
    val unzipped = input.map(parse).unzip
    Some(unzipped._1.sorted.zip(unzipped._2.sorted).map((a, b) => abs(a - b)).sum)

  override def solvePart2(input: List[String]): Option[Long] = Some(2)

  private val parse = (string: String) => string.split(" {3}").toList map (x => x.toInt) match
    case List(a, b) => (a, b)
    case _ => throw IllegalArgumentException()
      
