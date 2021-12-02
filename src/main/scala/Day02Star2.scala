package com.github.nikalaikina.aoc2021

object Day02Star2 extends Main[Long] {

  override def File = "inputs/day2.txt"

  override def solve(input: List[String]): List[Long] = {
    val (_, pos, depth) = input.foldLeft((0L, 0L, 0L)) {
      case ((aim, pos, depth), s"forward $x") => (aim, pos + x.toInt, depth + aim * x.toInt)
      case ((aim, pos, depth), s"up $x") => (aim - x.toInt, pos, depth)
      case ((aim, pos, depth), s"down $x") => (aim + x.toInt, pos, depth)
    }

    List(pos * depth)
  }
}
