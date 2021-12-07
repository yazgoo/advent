#!/bin/env amm
import scala.io.Source

@main
def main(dayOpt: Option[Int], partOpt: Option[Int]) = {
  val method_patern = """day(\d+)_(\d+)""".r
  val methods = getClass.getMethods.filter{ method => method_patern.matches(method.getName) }.map{ method  =>
    val method_patern(day, part) = method.getName
    (day.toInt, part.toInt, method)
  }.toList.sortBy(x => (x._1, x._2))

  val last = methods.last

  val day = dayOpt.getOrElse(last._1) 
  val part = partOpt.getOrElse(last._2)

  val groupedMethods = methods.groupBy { x => (x._1, x._2) }

  groupedMethods.get((day, part)) map { method => 
   method.head._3.invoke(this)
  }
}

def input(day: Int) = Source.fromFile(s"input/${day}").getLines

def day1_1 = input(1).map(_.toInt).sliding(2).map { case Seq(x, y, _*) => y > x }
  .filter{ x: Boolean => x }.size

def day1_2 = input(1).map(_.toInt).sliding(3).map { case Seq(x, y, z, _*) => x + y + z }.sliding(2).map { case Seq(x, y, _*) => y > x }
  .filter{ x: Boolean => x }.size

def day2_1 = { 
    def x_y = input(2).map{ x : String => 
    val s = x.split(" "); (s( 0 ), s( 1 ).toInt)
    }.foldLeft((0, 0)){ (acc, command) => command match {
    case ("forward", x) => (acc._1 + x, acc._2)
    case ("down", y) => (acc._1 , acc._2 + y)
    case ("up", y) => (acc._1 , acc._2 - y)
  }}
  x_y._1 * x_y._2
}

def day2_2 = { 
    def x_y = input(2).map{ x : String => 
    val s = x.split(" "); (s( 0 ), s( 1 ).toInt)
    }.foldLeft((0, 0, 0)){ (acc, command) => command match {
    case ("forward", x) => (acc._1 + x, acc._2 + acc._3 *x, acc._3)
    case ("down", y) => (acc._1 , acc._2, acc._3 + y) 
    case ("up", y) => (acc._1 , acc._2, acc._3 - y)
  }}
  x_y._1 * x_y._2
}

def day3_1 = {
  val in = input(3).toList
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

def day3_2 = {
  val in = input(3).toList.map { x : String => x.map(_.toString.toInt).toList}
  day3_filter(in, 0, true) * day3_filter(in, 0, false)
}

type BingoBoard = Seq[Seq[(Int, Boolean)]]

def day4_play_board(number: Int, board: BingoBoard) : BingoBoard = {
  board.map{ lines => lines.map{ x => if(x._1 == number) (number, true) else x } }
}

def day4_score(number: Int, board: BingoBoard) : Seq[Int] = {
    List(board.flatMap(_.filter(!_._2)).foldLeft(0){ (acc, i) => acc + i._1 } * number)
}

def day4_check_winner(number: Int, board: BingoBoard) : Boolean = {
  val lines = board.map{ line => line.filter{ x=> x._2 } }
  val cols = (0 to 4).map { col => (0 to 4).map { line => board(line)(col) }.filter(_._2) }
  (lines.filter(_.size == 5).size > 0 || cols.filter(_.size == 5).size > 0) 
}

def day4_play(numbers: Seq[Int], boards: Seq[BingoBoard]) : Seq[Int] = {
  numbers match {
    case number :: tail => {
      val boards_played = boards.map(day4_play_board(number, _))
      val winnerLoosers = boards_played.groupBy(day4_check_winner(number, _))
      val winners = winnerLoosers.getOrElse(true, Nil)
      val loosers = winnerLoosers.getOrElse(false, Nil)
      winners.flatMap(day4_score(number, _)) ++ day4_play(tail, loosers)
    }
    case x => {
      Nil
    }
  }
}

def day4 = {
  val in = input(4).toList
  in match {
    case numbers_s :: boards_list => {
      val numbers = numbers_s.split(",").map{_.toInt}.toList
      val boards = boards_list.filter(_ != "").grouped(5).toList.map(_.map{line => line.split(" ").filter{ a => a != "" }.map{ x => (x.toInt, false) }.toList})
      val answers = day4_play(numbers, boards)
      (/* 1stpart  */ answers.head, /* second part */ answers.last)
    }
  }
}

val day5_pattern = """(\d+),(\d+) -> (\d+),(\d+)""".r

def myto(a: Int, b: Int, size: Int) : Seq[Int] = {
  if (a < b) {
    a to b
  } else if (a == b) {
    (0 to size).map(_ => a)
  } else {
    (-a to -b).map(_ * -1)
  }
}

def day5_parse_segment(segment: String, diagonals: Boolean) : Seq[(Int, Int)] = {
  val day5_pattern(x1s, y1s, x2s, y2s) = segment
  val (x1, x2, y1, y2) = (x1s.toInt, x2s.toInt, y1s.toInt, y2s.toInt)
  val size = (x1.max(x2)-x1.min(x2)).max(y1.max(y2)-y1.min(y2))
  val points = myto(x1, x2, size).zip(myto(y1, y2, size))
  if ((x1 != x2 && y1 != y2) && !diagonals)
    Nil
  else
    points
}

def day5(diagonals: Boolean) = {
  val points = input(5).toList.flatMap(day5_parse_segment(_, diagonals))
  val twice = points.groupBy(identity).filter(_._2.size > 1)
  twice.size
}

def day5_1 = day5(false)

def day5_2 = day5(true)

def day6(days: Int) = {
  val fishes = input(6).toList{0}.split(",").map(_.toInt).groupBy(identity).map(x => (x._1, x._2.size.toLong)).toList
  (0 until days).foldLeft(fishes) { (fishes, i) =>
    fishes.flatMap(x => if (x._1 == 0) List((6, x._2), (8, x._2)) else List((x._1 - 1, x._2))).groupBy(_._1).map(x => (x._1, x._2.map(_._2).sum)).toList
  }.map(_._2).sum
}

def day6_1 = day6(80)

def day6_2 = day6(256)

def day7(transform: Int => Int) = {
  val positions = input(7).toList{0}.split(",").map(_.toInt)
  (positions.min to positions.max).map{ mv => positions.map{ pos => transform((pos-mv).abs)}.sum }.min
}

def day7_1 = day7(identity)

def day7_2 = day7{x => (x*(x+1))/2}
