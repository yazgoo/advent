#!/bin/env amm
import scala.io.Source

def day1_1_answer = Source.fromFile("day1_input_1")
  .getLines.map(_.toInt).sliding(2).map { case Seq(x, y, _*) => y > x }
  .filter{ x: Boolean => x }.size

def day1_2_answer = Source.fromFile("day1_input_1")
  .getLines.map(_.toInt).sliding(3).map { case Seq(x, y, z, _*) => x + y + z }.sliding(2).map { case Seq(x, y, _*) => y > x }
  .filter{ x: Boolean => x }.size

println(day1_2_answer)


