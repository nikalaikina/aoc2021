package com.github.nikalaikina.aoc2021

object Day04 extends Main[Int] {

  override def File = "inputs/day4.txt"

  override def solve(input: List[String]): List[Int] = {
    val nums = input.head.split(",").map(_.toInt)

    val boards = input.tail.grouped(6).toList.map(
      _.tail.map(
        _.split(" ").filter(_.nonEmpty).map(_.toInt).toList
      )
    )

    def run(set: Set[Int] = Set.empty, tail: List[Int] = nums.toList): Int = {
      val next = set + tail.head
      boards
        .map { b => check(next, b) }
        .collectFirst { case Some(sum) => sum * tail.head }
        .getOrElse(run(next, tail.tail))
    }


    def check(set: Set[Int], board: List[List[Int]]): Option[Int] = {
      if (board.exists(_.forall(set)) || board.transpose.exists(_.forall(set))) {
        Some(board.flatten.filterNot(set).sum)
      } else {
        None
      }
    }

    List(run())
  }
}
