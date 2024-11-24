package net.rohlandm.aoc24
package domain

trait DaySolver(day: Int):

  def solvePart1(): Option[Long] =
    None

  def solvePart2(): Option[Long] =
    None

  final def print(): Unit =
    println(
      s"""Solving Day ${day}...
         |Part 1: ${
        solvePart1() match
          case Some(value) => value
          case None => "Not implemented"
      }
         |Part 2: ${
        solvePart2() match
          case Some(value) => value
          case None => "Not implemented"
      }
         |""".stripMargin)


class NotImplemented(day: Int) extends DaySolver(day)

