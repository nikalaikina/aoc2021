package com.github.nikalaikina.aoc2021

object Day08Star2 extends Main[Int] {

  override def File = "inputs/day8.txt"

  /*
   1
  2 3
   4
  5 6
   7
   */

  val Digits = Map(
    Set(1, 2, 3, 5, 6, 7) -> 0,
    Set(3, 6) -> 1,
    Set(1, 3, 4, 5, 7) -> 2,
    Set(1, 3, 4, 6, 7) -> 3,
    Set(2, 3, 4, 6) -> 4,
    Set(1, 2, 4, 6, 7) -> 5,
    Set(1, 2, 4, 5, 6, 7) -> 6,
    Set(1, 3, 6) -> 7,
    Set(1, 2, 3, 4, 5, 6, 7) -> 8,
    Set(1, 2, 3, 4, 6, 7) -> 9,
  )

  val Letters = "abcdefg".toList

  def check(mapping: Map[Char, Int], ws: List[String]): Boolean = {
    ws.forall { w =>
      Digits.keys
        .filter(_.size == w.length)
        .exists(d => w.map(mapping(_)).forall(d.contains))
    }
  }

  override def solve(input: List[String]): List[Int] = {
    val result = input.map { s =>
      val in :: out :: Nil = s.split("\\|")
        .map(_.split(" ").filter(_.nonEmpty))
        .toList

      val words = (in ++ out).toList

      val Some(mapping) = Letters.permutations
        .map(_.zip(1 to 7).toMap)
        .find(mapping => check(mapping, words))

      out.map(w => Digits(w.map(mapping).toSet)).mkString.toInt
    }

    List(result.sum)
  }
}
