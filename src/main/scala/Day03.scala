package com.github.nikalaikina.aoc2021

object Day03 extends Main[Int] {

  override def File = "inputs/day3.txt"

  override def solve(input: List[String]): List[Int] = {
    val n = input.size

    val ones = input.map(_.toList).transpose.map(_.count(_ == '1'))

    val (g, e) = ones.reverse.zip(LazyList.iterate(1)(_ * 2)).foldLeft((0, 0)) {
      case ((g, e), (x, m)) if x > n / 2 => (g + m, e)
      case ((g, e), (_, m)) => (g, e + m)
    }

    List(g * e)
  }
}
