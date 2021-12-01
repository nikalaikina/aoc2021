package com.github.nikalaikina.aoc2021

import cats.effect.kernel.Resource
import cats.effect.{ExitCode, IO, IOApp}

import scala.io.Source

trait Main[T] extends IOApp {

  def File: String

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      lines <- Resource.fromAutoCloseable(IO(Source.fromFile(File)))
        .use(f => IO(f.getLines.toList))
      result = solve(lines)
      _ <- IO(result.foreach(println(_)))
    } yield ExitCode.Success
  }

  def solve(input: List[String]): List[T]

}
