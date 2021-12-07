package com.github.nikalaikina.aoc2021

object Day07 extends Main[Int] {

  override def File = "inputs/day7.txt"

  override def solve(input: List[String]): List[Int] = {
    val numbers = input.head.split(",").map(_.toInt)

    def calc(goal: Int): Int = numbers.map(x => math.abs(x - goal)).sum

    List((numbers.min to numbers.max).map(calc).min)
  }
}
