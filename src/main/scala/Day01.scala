package com.github.nikalaikina.aoc2021

object Day01 extends Main[Int] {

  override def File = "inputs/day1.txt"

  override def solve(input: List[String]): List[Int] = {
    val ints = input.map(_.toInt)

    val result = (ints zip ints.tail).count { case (a, b) => b > a }

    List(result)
  }
}
