package net.rohlandm.aoc24.domain

trait InputRepository:
  def getInput(day: Int): List[String]
