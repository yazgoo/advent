#!/bin/env amm
import scala.io.Source

def input(day: Int, input: Int) = Source.fromFile(s"day${day}_input_${input}").getLines

def day1_1_answer = Source.fromFile("day1_input_1")
  .getLines.map(_.toInt).sliding(2).map { case Seq(x, y, _*) => y > x }
  .filter{ x: Boolean => x }.size

def day1_2_answer = Source.fromFile("day1_input_1")
  .getLines.map(_.toInt).sliding(3).map { case Seq(x, y, z, _*) => x + y + z }.sliding(2).map { case Seq(x, y, _*) => y > x }
  .filter{ x: Boolean => x }.size

  def day2_1_answer = { 
      def x_y = input(2, 1).map{ x : String => 
      val s = x.split(" "); (s( 0 ), s( 1 ).toInt)
      }.foldLeft((0, 0)){ (acc, command) => command match {
      case ("forward", x) => (acc._1 + x, acc._2)
      case ("down", y) => (acc._1 , acc._2 + y)
      case ("up", y) => (acc._1 , acc._2 - y)
    }}
    x_y._1 * x_y._2
  }

  def day2_2_answer = { 
      def x_y = input(2, 1).map{ x : String => 
      val s = x.split(" "); (s( 0 ), s( 1 ).toInt)
      }.foldLeft((0, 0, 0)){ (acc, command) => command match {
      case ("forward", x) => (acc._1 + x, acc._2 + acc._3 *x, acc._3)
      case ("down", y) => (acc._1 , acc._2, acc._3 + y) 
      case ("up", y) => (acc._1 , acc._2, acc._3 - y)
    }}
    x_y._1 * x_y._2
  }



println(day2_2_answer)


