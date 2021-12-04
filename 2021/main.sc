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


def day3_1_answer = {
  val in = input(3, 1).toList
  val gamma = in.map { x : String => x.map(_.toString.toInt).toList}.foldLeft(List[Int]()) { (acc, bits) =>
    acc match {
      case Nil => bits
      case _ => acc.zip(bits).map{ case (a, b) => a + b }
    }
    }.map{ x => if(x > in.size / 2) 1 else 0 }.foldLeft(0) { (acc, bit) => acc << 1 | bit } 
  val epsilon = gamma ^ Integer.parseInt("111111111111", 2)
  gamma * epsilon
}

def day3_filter(measures: List[List[Int]], level: Int, more: Boolean) : Int = {
  if (measures.size <= 1) {
    measures{ 0 }.foldLeft(0) { (acc, bit) => acc << 1 | bit }
  } else {
    val count = measures.map(_( level )).foldLeft(0){ (acc, x) => acc + x }
    val x = if(!more) (0, 1) else (1, 0)
    val v = if ((measures.size % 2 == 0 && count == measures.size / 2) || count > measures.size / 2) x._1 else x._2
    day3_filter(measures.filter(_( level) == v), level + 1, more)
  }
}

def day3_2_answer = {
  val in = input(3, 1).toList.map { x : String => x.map(_.toString.toInt).toList}
  day3_filter(in, 0, true) * day3_filter(in, 0, false)
}

def day4_play_board(number: Int, board: Seq[Seq[(Int, Boolean)]]) : Seq[Seq[(Int, Boolean)]] = {
  board.map{ lines => lines.map{ x => if(x._1 == number) (number, true) else x } }
}

def day4_check_winner(number: Int, board: Seq[Seq[(Int, Boolean)]]) : Seq[Int] = {
  val lines = board.map{ line => line.filter{ x=> x._2 } }
  val cols = (0 to 4).map { col => (0 to 4).map { line => board(line)(col) }.filter(_._2) }
  if (lines.filter(_.size == 5).size > 0 || cols.filter(_.size == 5).size > 0) 
  {
    List(board.flatMap(_.filter(!_._2)).foldLeft(0){ (acc, i) => acc + i._1 } * number)
  }
  else Nil
}

def day4_play(numbers: Seq[Int], boards: Seq[Seq[Seq[(Int, Boolean)]]]) : Seq[Int] = {
  numbers match {
    case number :: tail => {
      val boards_played = boards.map(day4_play_board(number, _))
      boards_played.map(day4_check_winner(number, _)).flatten match {
        case l @ (head :: tail) => l
        case Nil => day4_play(tail, boards_played)
      }
    }
    case x => {
      Nil
    }
  }
}

def day4_1_answer = {
  val in = input(4, 1).toList
  in match {
    case numbers_s :: boards_list => {
      val numbers = numbers_s.split(",").map{_.toInt}.toList
      val boards = boards_list.filter(_ != "").grouped(5).toList.map(_.map{line => line.split(" ").filter{ a => a != "" }.map{ x => (x.toInt, false) }.toList})
      day4_play(numbers, boards)
    }
  }
}

println(day4_1_answer)


