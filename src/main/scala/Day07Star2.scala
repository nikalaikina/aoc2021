package com.github.nikalaikina.aoc2021

object Day07Star2 extends Main[Int] {

  override def File = "inputs/day7.txt"

  override def solve(input: List[String]): List[Int] = {
    val numbers = input.head.split(",").map(_.toInt)
    val cost = (1 to numbers.max).scan(0)(_ + _)

    def calc(goal: Int): Int = numbers.map(x => cost(math.abs(x - goal))).sum

    List((numbers.min to numbers.max).map(calc).min)
  }
}
