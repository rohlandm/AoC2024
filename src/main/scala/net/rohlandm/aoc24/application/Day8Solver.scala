package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.domain.DaySolver

import scala.math.{abs, max, min}

class Day8Solver extends DaySolver(8):
  override def solvePart1(input: List[String]): Option[Long] =
    val maxX = input.head.length
    val maxY = input.length
    val map = input.zipWithIndex.flatMap(parse).toVector
    val grouped = map.groupBy(_.antenna)
    val antinodes = grouped.view.filterKeys(_.isDefined).values
      .map(generatePairs)
      .flatMap(x => x.flatMap(calculateAntinodes)).toList
    println(antinodes)
    println(antinodes.filter(checkBoundaries(_, maxX, maxY)))
    println(antinodes.distinct.filter(checkBoundaries(_, maxX, maxY)))
    Some(antinodes.distinct.count(checkBoundaries(_, maxX, maxY)))

  private def checkBoundaries(location: Location, maxX: Int, maxY: Int): Boolean =
    0.until(maxX).contains(location.x) && 0.until(maxY).contains(location.y)

  private def calculateAntinodes(antennas: (Location, Location)): Set[Location] =
    val diffs = (abs(antennas._1.x - antennas._2.x), abs(antennas._1.y - antennas._2.y))
    val lowers = (min(antennas._1.x, antennas._2.x), min(antennas._1.y, antennas._2.y))
    val uppers = (max(antennas._1.x, antennas._2.x), max(antennas._1.y, antennas._2.y))
    antennas match
      case pair if (pair._1.x > pair._2.x && pair._1.y > pair._2.y) ||
        (pair._1.x < pair._2.x && pair._1.y < pair._2.y)=> Set(
        Location(lowers._1 - diffs._1, lowers._2 - diffs._2),
        Location(uppers._1 + diffs._1, uppers._2 + diffs._2))
      case pair => Set(
        Location(uppers._1 + diffs._1, lowers._2 - diffs._2),
        Location(lowers._1 - diffs._1, uppers._2 + diffs._2))

  private def generatePairs(locations: Seq[Location]): Seq[(Location, Location)] =
    locations.combinations(2).map {
      case Seq(a, b) => (a, b)
    }.toSeq

  private def parse(line: String, y: Int): Vector[Location] =
    line.toCharArray.zipWithIndex.map((char, x) => Location(x = x, y = y, antenna = char match
      case '.' => None
      case c => Some(c)
    )).toVector

case class Location(x: Int, y: Int, antenna: Option[Frequency] = None)

type Frequency = Char