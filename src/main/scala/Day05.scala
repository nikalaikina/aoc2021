package com.github.nikalaikina.aoc2021

object Day05 extends Main[Int] {

  override def File = "inputs/day5.txt"

  type Pair = (Int, Int)

  override def solve(input: List[String]): List[Int] = {
    val ranges: List[(Pair, Pair)] = input
      .map { case s"$x1,$y1 -> $x2,$y2" => (x1.toInt, y1.toInt) -> (x2.toInt, y2.toInt) }

    @scala.annotation.tailrec
    def rec(list: List[(Pair, Pair)] = ranges, set: Set[Pair] = Set.empty, set2: Set[Pair] = Set.empty): Int = {
      list match {
        case ((x1, y1), (x2, y2)) :: tail if x1 == x2 || y1 == y2 =>
          val pairs = for {
            x <- (x1 min x2) to (x1 max x2)
            y <- (y1 min y2) to (y1 max y2)
          } yield (x, y)

          rec(tail, set ++ pairs, set2 ++ pairs.filter(set))
        case _ :: tail => rec(tail, set, set2)
        case Nil => set2.size
      }
    }

    List(rec(ranges))
  }
}
