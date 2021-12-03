package com.github.nikalaikina.aoc2021

object Day03Star2 extends Main[Int] {

  override def File = "inputs/day3.txt"

  override def solve(input: List[String]): List[Int] = {
    val ones: List[List[Int]] = input.map(_.toList).map(_.map(x => if (x == '0') 0 else 1))

    val majority: List[Int] => Int = { list =>
      if (list.sum >= (list.size + 1) / 2) 1 else 0
    }
    val minority: List[Int] => Int = majority.andThen(1 - _)

    val oxygen = find(ones, majority)
    val co2 = find(ones, minority)

    List(toDec(oxygen.reverse) * toDec(co2.reverse))
  }

  def toDec(list: List[Int]): Int = {
    (list zip LazyList.iterate(1)(_ * 2)).map { case (a, b) => a * b }.sum
  }

  @scala.annotation.tailrec
  def find(list: List[List[Int]], f: List[Int] => Int, i: Int = 0): List[Int] = {
    list match {
      case head :: Nil => head
      case list =>
        val criteria = f(list.map(_ (i)))
        find(list.filter(_ (i) == criteria), f, i + 1)
    }
  }
}
