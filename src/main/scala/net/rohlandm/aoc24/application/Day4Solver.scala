package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.domain.DaySolver

import scala.language.postfixOps


class Day4Solver extends DaySolver(4):

  private val xmas = """XMAS""".r

  override def solvePart1(input: List[String]): Option[Long] =
    val paddedInput = padInput.apply(input)

    val forward = paddedInput flatMap (xmas.findAllIn(_)) size
    val backward = paddedInput map (_.reverse) flatMap (xmas.findAllIn(_)) size
    val downward = stringTranspose.apply(paddedInput) flatMap (xmas.findAllIn(_)) size
    val upward = stringTranspose.apply(paddedInput) map (_.reverse) flatMap (xmas.findAllIn(_)) size
    val diagonalBackwardDownward = stringTranspose
      .apply(paddedInput.zipWithIndex.map((value, idx) => shiftString(value, idx)))
      .flatMap(xmas.findAllIn)
      .size
    val diagonalForwardUpward = stringTranspose
      .apply(paddedInput.reverse.zipWithIndex.map((value, idx) => shiftString(value, idx)))
      .flatMap(xmas.findAllIn)
      .size
    val diagonalForwardDownward = stringTranspose
      .apply(paddedInput.zipWithIndex.map((value, idx) => shiftString(value, -idx)))
      .flatMap(xmas.findAllIn)
      .size
    val diagonalBackwardUpward = stringTranspose
      .apply(paddedInput.reverse.zipWithIndex.map((value, idx) => shiftString(value, -idx)))
      .flatMap(xmas.findAllIn)
      .size
    Some(forward +
      backward +
      downward +
      upward +
      diagonalForwardUpward +
      diagonalForwardDownward +
      diagonalBackwardUpward +
      diagonalBackwardDownward)


  private val padInput = (input: List[String]) =>
    val padLine = "#" * (input.head.length + 8)
    List(padLine, padLine, padLine, padLine) ++
      input.map("####" + _ + "####") ++
      List(padLine, padLine, padLine, padLine)

  private val stringTranspose = (strings: List[String]) =>
    strings.map(_.toCharArray.toList).transpose.map(_.mkString)

  private val shiftString = (string: String, shift: Int) =>
    shift match
      case x if x > 4 => string.substring(string.length - x + 4, string.length) + string.substring(0, string.length - x + 4)
      case x if x < -4 => string.substring(-x - 4) + string.substring(0, -x - 4)
      case _ => string
    