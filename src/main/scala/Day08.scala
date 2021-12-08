package com.github.nikalaikina.aoc2021

object Day08 extends Main[Int] {

  override def File = "inputs/day8.txt"

  override def solve(input: List[String]): List[Int] = {
    val unique = Set(2, 4, 3, 7)

    val result = input
      .flatMap(_.split("\\|")(1).split(" ").toList)
      .count(w => unique(w.length))

    List(result)
  }
}
