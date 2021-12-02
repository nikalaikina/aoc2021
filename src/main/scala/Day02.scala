package com.github.nikalaikina.aoc2021

object Day02 extends Main[Int] {

  override def File = "inputs/day2.txt"

  override def solve(input: List[String]): List[Int] = {
    val (pos, depth) = input.foldLeft((0, 0)) {
      case ((pos, depth), s"forward $x") => (pos + x.toInt, depth)
      case ((pos, depth), s"up $x") => (pos, depth - x.toInt)
      case ((pos, depth), s"down $x") => (pos, depth + x.toInt)
    }

    List(pos * depth)
  }
}
