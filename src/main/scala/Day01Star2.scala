package com.github.nikalaikina.aoc2021

object Day01Star2 extends Main[Int] {

  override def File = "inputs/day1.txt"

  val Window = 3

  override def solve(input: List[String]): List[Int] = {
    val ints = input.map(_.toInt)

    val windows = ints.tails.toList
      .map(_.take(Window))
      .filter(_.size == Window)
      .map(_.sum)

    val result = (windows zip windows.tail).count { case (a, b) => b > a }

    List(result)
  }
}
