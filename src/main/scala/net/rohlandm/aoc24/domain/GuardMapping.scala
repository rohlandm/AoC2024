package net.rohlandm.aoc24.domain

case class GuardMapping(grid: List[List[MapCoordinate]], guard: Guard, loop: Boolean)

case class Guard(x: Int, y: Int, direction: Direction)

enum MapCoordinate:
  case OBSTRUCTION
  case FREE(visited: Set[Direction])

enum Direction:
  case UP,DOWN,LEFT,RIGHT
