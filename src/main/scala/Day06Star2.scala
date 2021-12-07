package com.github.nikalaikina.aoc2021

import cats.implicits._

object Day06Star2 extends Main[Long] {

  override def File = "inputs/day6.txt"

  override def solve(input: List[String]): List[Long] = {

    val init = input.head.split(",")
      .map(_.toInt)
      .groupBy(identity)
      .view
      .mapValues(_.length.toLong)
      .toMap

    val result = LazyList.iterate(init) { map =>
      val zeroes = map.getOrElse(0, 0L)
      (map - 0).map { case (k, v) => k - 1 -> v } |+| Map(6 -> zeroes, 8 -> zeroes)
    }

    List(result(256).values.sum)
  }
}
