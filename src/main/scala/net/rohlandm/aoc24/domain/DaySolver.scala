package net.rohlandm.aoc24
package domain

import java.time.Instant

trait DaySolver(day: Int):

  def solvePart1(input: List[String]): Option[Long] =
    None

  def solvePart2(input: List[String]): Option[Long] =
    None

  final def print(input: List[String]): Unit =
    
    val start = Instant.now
    val result1 = solvePart1(input)
    val result2 = solvePart2(input)
    val end = Instant.now
    
    println(
      s"""🎄🎄🎄🎄🎄🎄🎄🎄🎄🎄🎄🎄
         |🎅Solving Day $day...
         |🛷Part 1: ${
        solvePart1(input) match
          case Some(value) => value
          case None => "Not implemented"
      }
         |🛷Part 2: ${
        solvePart2(input) match
          case Some(value) => value
          case None => "Not implemented"
      }
         |🎄🎄🎄🎄🎄🎄🎄🎄🎄🎄🎄🎄
         |Calculation time: ${end.toEpochMilli - start.toEpochMilli}ms
         |""".stripMargin)


class NotImplemented(day: Int) extends DaySolver(day)

