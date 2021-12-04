package com.github.nikalaikina.aoc2021

import cats.instances.list._


object Day04Star2 extends Main[Int] {

  override def File = "inputs/day4.txt"

  override def solve(input: List[String]): List[Int] = {
    val nums = input.head.split(",").map(_.toInt)

    val boards: List[List[List[Int]]] = input.tail.grouped(6).toList.map(
      _.tail.map(_.split(" ").filter(_.nonEmpty).map(_.toInt).toList)
    )

    @scala.annotation.tailrec
    def run(set: Set[Int] = Set.empty, tail: List[Int] = nums.toList, boards: List[List[List[Int]]] = boards): Int = {
      val next = set + tail.head
      val nextBoards = boards.filterNot(won(next, _))

      if (nextBoards.isEmpty) {
        sum(next, boards.last) * tail.head
      } else {
        run(next, tail.tail, nextBoards)
      }
    }

    def sum(set: Set[Int], board: List[List[Int]]): Int = {
      board.flatten.filterNot(set).sum
    }

    def won(set: Set[Int], board: List[List[Int]]): Boolean = {
      board.exists(_.forall(set)) || board.transpose.exists(_.forall(set))
    }

    List(run())
  }

}
