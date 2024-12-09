package net.rohlandm.aoc24.application

import net.rohlandm.aoc24.domain.Direction.{DOWN, LEFT, RIGHT, UP}
import net.rohlandm.aoc24.domain.MapCoordinate.{FREE, OBSTRUCTION}
import net.rohlandm.aoc24.domain.{DaySolver, Direction, Guard, GuardMapping, MapCoordinate}

import scala.annotation.tailrec

class Day6Solver extends DaySolver(6):
  override def solvePart1(input: List[String]): Option[Long] =
    val guardMapping = parse(input)
    val simulated = simulate(guardMapping)
    Some(simulated.grid.flatten.map {
      case MapCoordinate.FREE(visited) if visited.nonEmpty => 1
      case _ => 0
    }.sum)

  override def solvePart2(input: List[String]): Option[Long] =
    val guardMapping = parse(input)
    val simulated = simulate(guardMapping)
    val visited = simulated.grid.zipWithIndex.flatMap((row, yIdx) => row.zipWithIndex
        .map((coordinate, xIdx) => (coordinate, yIdx, xIdx)))
      .filter((value, _, _) => value match
        case MapCoordinate.OBSTRUCTION => false
        case MapCoordinate.FREE(visited) => visited.nonEmpty)
      .filterNot((_, y, x) => guardMapping.guard.y == y && guardMapping.guard.x == x)
    val obstructed = visited.map((_, y, x) =>
      GuardMapping(grid = guardMapping.grid.updated(y, guardMapping.grid.apply(y).updated(x, OBSTRUCTION)),
        guard = guardMapping.guard,
        loop = guardMapping.loop)
    )
    val loops = obstructed.map(simulate).filter(_.loop)
    Some(loops.distinct.size)

  @tailrec
  private def simulate(guardMapping: GuardMapping): GuardMapping =
    guardMapping.guard match
      case Guard(x, y, DOWN) if y > guardMapping.grid.size - 1 => guardMapping
      case Guard(x, y, UP) if y < 0 => guardMapping
      case Guard(x, y, LEFT) if x < 0 => guardMapping
      case Guard(x, y, RIGHT) if x > guardMapping.grid.head.size - 1 => guardMapping
      case Guard(x, y, dir) if loopEval(guardMapping.grid.apply(y).apply(x), dir, (x, y)) => GuardMapping(grid = guardMapping.grid, guard = guardMapping.guard, loop = true)
      case _ =>
        val newMapping = moveGuard(guardMapping)
        simulate(newMapping)

  private def loopEval(coordinate: MapCoordinate, direction: Direction, coordinates: (Int, Int)): Boolean =
    coordinate match
      case MapCoordinate.OBSTRUCTION => throw IllegalStateException(s"""Guard on Obstruction at x=${coordinates._1}, y=${coordinates._2}""")
      case MapCoordinate.FREE(visited) => visited.contains(direction)

  private def moveGuard(mapping: GuardMapping): GuardMapping =
    mapping.guard match
      case Guard(x, y, DOWN) =>
        GuardMapping(grid = visit(mapping, x, y), guard = mapping.guard match
          case _ if y == mapping.grid.size - 1 => Guard(x, y + 1, DOWN)
          case _ if mapping.grid.apply(y + 1).apply(x) == OBSTRUCTION => moveGuard(GuardMapping(grid = mapping.grid,
            guard = Guard(x = mapping.guard.x, y = mapping.guard.y, direction = LEFT),
            loop = mapping.loop)).guard
          case _ => Guard(x, y + 1, DOWN), mapping.loop)
      case Guard(x, y, UP) => GuardMapping(grid = visit(mapping, x, y), guard = mapping.guard match
        case _ if y == 0 => Guard(x, y - 1, UP)
        case _ if mapping.grid.apply(y - 1).apply(x) == OBSTRUCTION => moveGuard(GuardMapping(grid = mapping.grid,
          guard = Guard(x = mapping.guard.x, y = mapping.guard.y, direction = RIGHT),
          loop = mapping.loop)).guard
        case _ => Guard(x, y - 1, UP), mapping.loop)
      case Guard(x, y, LEFT) => GuardMapping(grid = visit(mapping, x, y), guard = mapping.guard match
        case _ if x == 0 => Guard(x - 1, y, LEFT)
        case _ if mapping.grid.apply(y).apply(x - 1) == OBSTRUCTION => moveGuard(GuardMapping(grid = mapping.grid,
          guard = Guard(x = mapping.guard.x, y = mapping.guard.y, direction = UP),
          loop = mapping.loop)).guard
        case _ => Guard(x - 1, y, LEFT), mapping.loop)
      case Guard(x, y, RIGHT) => GuardMapping(grid = visit(mapping, x, y), guard = mapping.guard match
        case _ if x == mapping.grid.head.size - 1 => Guard(x + 1, y, RIGHT)
        case _ if mapping.grid.apply(y).apply(x + 1) == OBSTRUCTION => moveGuard(GuardMapping(grid = mapping.grid,
          guard = Guard(x = mapping.guard.x, y = mapping.guard.y, direction = DOWN),
          loop = mapping.loop)).guard
        case _ => Guard(x + 1, y, RIGHT), mapping.loop)

  private def visit(mapping: GuardMapping, x: Int, y: Int): Vector[Vector[MapCoordinate]] =
    mapping.grid.zipWithIndex.map((row, yIdx) => yIdx match
      case idx if idx == y => row.updated(x, row.apply(x) match
        case MapCoordinate.FREE(visited) => FREE(Set.newBuilder.addAll(visited).addOne(mapping.guard.direction).result())
        case MapCoordinate.OBSTRUCTION => throw IllegalStateException(s"""visiting Obstruction at x=$x, y=$y, $mapping""")
      )
      case _ => row)

  private def parse(input: List[String]): GuardMapping =
    var guard: Option[Guard] = None
    val grid = input.map(_.toCharArray.toList).zipWithIndex.map((charList, yIdx) => charList.zipWithIndex.map((char, xIdx) => {
      char match
        case '.' => FREE(Set.empty)
        case '#' => OBSTRUCTION
        case '^' =>
          guard = Some(Guard(xIdx, yIdx, UP))
          FREE(Set.empty)
    }).toVector).toVector
    guard match
      case Some(value) => GuardMapping(grid, value, false)
      case None => throw IllegalArgumentException("Input without guard")
