package com.github.nikalaikina.aoc2021

object Day06 extends Main[Int] {

  override def File = "inputs/day6.txt"

  override def solve(input: List[String]): List[Int] = {

    val iterations = LazyList.iterate(List(8)) {
      _.flatMap {
        case 0 => List(6, 8)
        case x => List(x - 1)
      }
    }.take(90).map(_.size).toVector

    val result = input.head.split(",")
      .map(_.toInt)
      .map(x => iterations(88 - x))
      .sum

    List(result)
  }
}
