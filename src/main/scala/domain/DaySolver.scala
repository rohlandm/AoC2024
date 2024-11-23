package net.rohlandm.aoc24
package domain

trait DaySolver:

  def day(): String

  def solvePart1(): Option[Long] =
    Option.empty

  def solvePart2(): Option[Long] =
    Option.empty

  final def print(): Unit =
    println(
      s"""Solving Day ${day()}...
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


class NotImplemented(dayNumber: Int) extends DaySolver:
  override def day(): String = dayNumber.toString

