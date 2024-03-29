#!./ammonite
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

def day8_1 = input(8).toList.flatMap(_.split("\\|").last.split(" ")).filter{ x => List(2, 3, 4, 7).contains(x.size) }.size

def day8_find_sized(firstPart: Array[Array[Char]], size: Int) : Seq[Char] = {
  firstPart.filter(x => x.size == size)(0).toList
}

def list_contains_items(list: Seq[Char], items: Seq[Char]) : Boolean = 
  items.map(list.contains(_)).foldLeft(true){ (acc, v) => acc && v }

def list_matching_items(list: Seq[Char], items: Seq[Char]) : Int = 
  items.map{ x => if (list.contains(x)) 1 else 0 }.foldLeft(0){ (acc, v) => acc + v }

def day8_filter_first(multiple: Array[Array[Char]], condition: Array[Char] => Boolean) = {
    multiple.filter(condition)(0).toList
}

def day8_digits(firstPart: Array[Array[Char]]) = {
    // 1, 4, 7, 8 have a unique number of wire:
    val one = day8_find_sized(firstPart, 2)
    val four = day8_find_sized(firstPart, 4)
    val seven = day8_find_sized(firstPart, 3)
    val eight = day8_find_sized(firstPart, 7)
    // 9, 6, 0 have a number of wire of 6
    val nine_six_zero = firstPart.filter(x => x.size == 6)
    // 6 is the only one which does not contain all wires of 1
    val six = day8_filter_first(nine_six_zero, { x => !list_contains_items(x, one) })
    // 9 and 0 are the remainings
    val nine_zero = nine_six_zero.filter { x => list_contains_items(x, one) } 
    // 9 conatins all the wires of 4
    val nine = day8_filter_first(nine_zero, { x => list_contains_items(x, four) })
    // 0 doesn't
    val zero = day8_filter_first(nine_zero, { x => !list_contains_items(x, four) })
    // 2, 3, 5 have a number of wire of 5
    val two_three_five = firstPart.filter(x => x.size == 5)
    // 3 is the only one containing all wires of 1
    val three = day8_filter_first(two_three_five, { x => list_contains_items(x, one) })
    val two_five = two_three_five.filter{ x => !list_contains_items(x, one) }
    // 2 has 2 wires in common with 4
    val two = day8_filter_first(two_five, { x => list_matching_items(x, four) == 2 })
    // 5 has 3 wires in common with 4
    val five = day8_filter_first(two_five, { x => list_matching_items(x, four) == 3 })
    List(zero, one, two, three, four, five, six, seven, eight, nine)
}

def day8_2 = {
  input(8).toList.map { line =>
      val parts = line.split(" \\| ")
      val uniqueSignalPatterns = parts(0).split(" ").map(_.toCharArray.sorted)
      val digits = day8_digits(uniqueSignalPatterns)
      val outputValue = parts(1).split(" ").map(_.toCharArray.sorted.toList)
      outputValue.map { number => digits.indexOf(number) }.foldLeft(0){ (acc, x) => x + 10 * acc}
    }.sum
}

def day9_heightMap(i: Int) = 
  input(i).toList.map(x => x.toCharArray.map(c => c.toString.toInt).toList)

def day9_low_points(heightMap: List[List[Int]]) = 
  heightMap.zipWithIndex.flatMap { case (line, y) =>
    line.zipWithIndex.map { case (height, x) => 
      if(List((x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1))
        .filter{ case (x, y) => x >= 0 && y >= 0 && x < line.size && y < heightMap.size }
        .filter{ case (x, y) => heightMap(y)(x) <= height }.size == 0) {
          Some((x, y), height + 1)
        } else {
          None
        }
    }
  }.flatten

def day9_1 = day9_low_points(day9_heightMap(9)).map(_._2).sum

// looks like a flood fill algo
// https://www.freecodecamp.org/news/flood-fill-algorithm-explained/
def day9_basin_points(acc: List[(Int, Int)], pos: (Int, Int), heightMap: List[List[Int]]) : List[(Int, Int)] = {
  val (x, y) = pos
  if (acc.contains(pos) || x < 0 || y < 0 || x >= heightMap(0).size || y >= heightMap.size || heightMap(y)(x) == 9) {
    acc
  } else {
    val acc2 = acc ++ List(pos)
    val up = day9_basin_points(acc2, (x, y + 1), heightMap)
    val down = day9_basin_points(up, (x, y - 1), heightMap)
    val left = day9_basin_points(down, (x - 1, y), heightMap)
    val right = day9_basin_points(left, (x + 1, y), heightMap)
    right
  }
}

def day9_2 = {
  val heightMap = day9_heightMap(9)
  val lowPoints = day9_low_points(heightMap).map(_._1)
  lowPoints.map { case (x, y) =>
    day9_basin_points(Nil, (x, y), heightMap).size
  }.sorted.reverse.take(3).reduce((a,b) => a * b)
}

def day10_check(line: Seq[Char], stack: Seq[Int]) : Int = {
  val open = List(
    '(', 
    '[', 
    '{', 
    '<', 
    )
  val close = List(
    ')',
    ']',
    '}',
    '>',
    )
  val scores = List(
    3,
    57,
    1197,
    25137,
    )
  line match {
    case char :: queue => {
      val opener = open.indexOf(char)
      if (opener != -1) {
        day10_check(queue, List(opener) ++ stack)
      } else {
        val closer = close.indexOf(char)
        if (closer != -1) {
          stack match {
            case opener :: rest => {
              if (opener == closer) {
                day10_check(queue, rest)
              }
              else {
                scores(closer)
              }
            }
            case Nil => 
              0
          }
        } else {
          0
        }
      }
    }
    case Nil => {
      0
    }
  }
}

def day10_1 = {
  input(10).toList.map { line =>
      day10_check(line.toCharArray.toList, Nil)
    }.sum
}

def day10_check_2(line: Seq[Char], stack: Seq[Int]) : Long = {
  val open = List(
    '(', 
    '[', 
    '{', 
    '<', 
    )
  val close = List(
    ')',
    ']',
    '}',
    '>',
    )
  line match {
    case char :: queue => {
      val opener = open.indexOf(char)
      if (opener != -1) {
        day10_check_2(queue, List(opener) ++ stack)
      } else {
        val closer = close.indexOf(char)
        if (closer != -1) {
          stack match {
            case opener :: rest => {
              if (opener == closer) {
                day10_check_2(queue, rest)
              }
              else {
                0
              }
            }
            case Nil => 
              0
          }
        } else {
          0
        }
      }
    }
    case Nil => {
      stack.foldLeft(0L) { (acc, opener) =>
        acc * 5L + (opener + 1L)
      }
    }
  }
}

def day10_2 = {
  val scores = input(10).toList.map { line =>
      day10_check_2(line.toCharArray.toList, Nil)
    }.filter(_ != 0).sorted
    scores(scores.size / 2)
}

def day11_run(flashes: List[(Int, Int)], energies: List[List[Int]]) : (List[List[Int]], List[(Int, Int)]) = {
    val newFlashes = energies.zipWithIndex.flatMap { line_y =>
      line_y._1.zipWithIndex.flatMap { item_x =>
        if (item_x._1 > 9) {
          Some((line_y._2, item_x._2))
        } else {
          None
        }
      }
    }.filter(!flashes.contains(_))
    if (newFlashes.size == 0) {
      (energies, flashes)
    } else {
    val withAdjacents = newFlashes.foldLeft(energies){ case (acc, (y, x)) =>
      val adjacents = List((y - 1, x), (y + 1, x), (y, x - 1), (y, x + 1), (y + 1, x + 1), (y - 1, x - 1), (y + 1, x - 1), (y - 1, x + 1)).filter{ case (y, x) => 0 <= y && y < energies.size && 0 <= x && x < energies(0).size }
      adjacents.foldLeft(acc) { case (acc2, (y2, x2)) =>
        val line = acc2(y2)
        acc2.updated(y2, line.updated(x2, line(x2) + 1))
      }
    }
    day11_run(flashes ++ newFlashes, withAdjacents)
    }
}

def day11_resetFlashes(newEnergies: List[List[Int]], flashes: List[(Int, Int)]) = 
  flashes.foldLeft(newEnergies) { (acc, flash) => acc.updated(flash._1, acc(flash._1).updated(flash._2, 0)) }

lazy val day11_startEnergies = input(11).toList.map(l => l.map(_.toString.toInt).toList)

def day11_1 = {
  (0 until 100).foldLeft((day11_startEnergies, 0)) { (energiesFlashesCount, _) =>
    val (energies, flashesCount) = energiesFlashesCount
    val increasedEnergies = energies.map { line => line.map(_ + 1) }
    val (newEnergies, flashes) = day11_run(Nil, increasedEnergies)
    (day11_resetFlashes(newEnergies, flashes), flashesCount + flashes.size)
  }
}

def day11_run2(energies: List[List[Int]], flashesCount: Int, i: Int) : Int = {
  val increasedEnergies = energies.map { line => line.map(_ + 1) }
  val (newEnergies, flashes) = day11_run(Nil, increasedEnergies)
  if (flashes.size == energies.size * energies(0).size) {
    i
  } else day11_run2(day11_resetFlashes(newEnergies, flashes), flashesCount + flashes.size, i + 1)
}

def day11_2 = day11_run2(day11_startEnergies, 0, 1)

def day12_is_small_cave(x: String) = x.toLowerCase() == x

def day12_find_paths(connections: Map[String, List[String]], smallCaves: List[String], path: List[String]) : List[List[String]] = {
  if(smallCaves.size == 0) {
    Nil
  } else {
    connections.get(path.last) match {
      case None => Nil
      case Some(caves) => caves.flatMap { cave =>
        cave match {
          case "end" => List(path ++ List("end"))
          case other if(day12_is_small_cave(other)) => {
            if(smallCaves.contains(other)) {
              day12_find_paths(connections, smallCaves.filter(_ != other).toList, path ++ List(other))
            } else {
              Nil
            }
          }
          case other => 
              day12_find_paths(connections, smallCaves, path ++ List(other))
        }
      }
    }
  }
}

lazy val day12_connections = 
  input(12).toList.map(_.split("-")).flatMap(x => List((x{0}, x{1}), (x{1}, x{0})))
    .groupBy(x => x._1).mapValues(x => x.map(_._2)).toMap

def day12_1 = {
  val smallCaves = day12_connections.keys.filter(day12_is_small_cave).toList
  day12_find_paths(day12_connections, smallCaves.filter(_ != "start").toList, List("start")).size
}

def day12_2_find_paths(connections: Map[String, List[String]], smallCavesVisited: List[String], path: List[String]) : List[List[String]] = {
  connections.get(path.last) match {
    case None => Nil
    case Some(caves) => caves.flatMap { cave =>
      cave match {
        case "start" => Nil
        case "end" => List(path ++ List("end"))
        case other if(day12_is_small_cave(other)) => {
          if(smallCavesVisited.contains(other) && smallCavesVisited.distinct != smallCavesVisited) {
            Nil
          } else {
            day12_2_find_paths(connections, smallCavesVisited ++ List(other), path ++ List(other))
          }
        }
        case other => 
            day12_2_find_paths(connections, smallCavesVisited, path ++ List(other))
      }
    }
  }
}

def day12_2 = day12_2_find_paths(day12_connections, Nil, List("start")).size

def day13_fold(points: List[(Int, Int)], fold: (String, Int)) : List[(Int, Int)] = {
  fold match {
    case ("y", i) => 
      points.map { case (x, y) =>
        if (y > i)
          (x, 2 * i - y)
        else
          (x, y)
      }.distinct
    case ("x", i) => 
      points.map { case (x, y) =>
        if (x > i)
          (2 * i - x, y)
        else
          (x, y)
      }.distinct
  }
}

def day13_parse(i: Int) = {
  val coordinates = """(\d+),(\d+)""".r
  val foldInstruction = """fold along (.)=(\d+)""".r
  val in = input(i).toList
  val points = in.filter(coordinates.matches(_)).map{ str => val coordinates(x, y) = str; (x.toInt, y.toInt) }
  val folds = in.filter(foldInstruction.matches(_)).map{ str => val foldInstruction(axis, value) = str; (axis, value.toInt) }
  (points, folds)
}

def day13_1 = {
  val (points, folds) = day13_parse(13)
  day13_fold(points, folds(0)).length
}

def day13_2 = {
  print("\u001Bc") // clear screen first
  val (points, folds) = day13_parse(13)
  folds.foldLeft(points) { (acc, fold) => 
    day13_fold(acc, fold)
  }.sorted.map{ case (x, y) =>
  println(s"\u001B[${y + 10};${x*2}H██") }
  println(s"\u001B[0;0H")
  ()
}

def day14_1 = {
  val in = input(14).toList
  val template = in(0).toList
  val rules = in.drop(2).map(_.split(" -> ")).map(x => (List(x{0}{0}, x{0}{1}), x{1})).toMap
  val result = (1 to 10).foldLeft(template) { (acc, _) =>
    acc.sliding(2).toList.flatMap { pair =>
      rules.get(pair) match {
        case Some(polym) => List(pair{0}, polym{0})
        case None => pair.toList
      }
    } ++ List(template.last)
  }
  val ordered = result.groupBy(identity).mapValues(_.size)
  ordered.maxBy(_._2)._2 - ordered.minBy(_._2)._2
}

def day14_2 = {
  val in = input(14).toList
  val template = in(0).toList.sliding(2).toList.groupBy{x => (x{0}, x{1})}.mapValues(_.size.toLong)
  val firstLast = List(in(0){0}, in(0).last)
  val rules = in.drop(2).map(_.split(" -> ")).map(x => ((x{0}{0}, x{0}{1}), x{1}{0})).toMap
  val result = (1 to 40).foldLeft(template) { (acc, _) =>
     acc.flatMap { case (pair, count) =>
      rules.get(pair) match {
        case Some(polym) => List(((pair._1, polym), count), ((polym, pair._2), count))
        case None => List((pair, count))
      }
    }.groupBy(_._1).mapValues(_.map(_._2).sum)
  }
  val ordered = result.flatMap(x => List((x._1._1, x._2), (x._1._2, x._2))).groupBy(_._1).mapValues(_.map(_._2).sum)
  val max = ordered.maxBy(_._2)
  val min = ordered.minBy(_._2)
  val max2 = if (firstLast.contains(max._1)) max._2 + 1 else max._2
  val min2 = if (firstLast.contains(min._1)) min._2 + 1 else min._2
  (max2 - min2) /2
}

def find_lowest_risk_dumb(risks: List[List[Int]], position: (Int, Int), total: Int, bestOpt: Option[Int]) : Option[Int] = {
      val height = risks.size
      val width = risks(0).size
      val (y, x) = position
      val total2 = total + risks(y)(x)
      bestOpt match {
        case Some(best) if best <= total2 => Some(best)
        case _ => {
          if (position == (height - 1, width - 1)) {
            Some(total2)
          } else {
            List((y + 1, x), (y, x + 1))
              .filter{ case (y, x) => y < height && x < width }
              .foldLeft(bestOpt){ case (acc, (y2, x2)) => find_lowest_risk_dumb(risks, (y2, x2), total2, acc) }
          }
        }
      }
}


type Coords = (Int, Int)

import scala.collection.mutable

def unvisitedVertexWithSmallestKnownDistance(unvisited: mutable.ArrayDeque[Coords], shortestDistance: mutable.Map[Coords, (Int, Boolean)]) : Option[(Coords, (Int, Boolean))] = {
  //unvisited.flatMap(x => shortestDistance.get(x).map((x, _))).minByOption(_._2)
  shortestDistance.filter(!_._2._2).minByOption(_._2._1)
}

def unvisitedNeighbourgs(unvisited: mutable.ArrayDeque[Coords], vertex: Coords, shortestDistance: mutable.Map[Coords, (Int, Boolean)]) : List[Coords] = {
  List((vertex._1, vertex._2 + 1), (vertex._1 + 1, vertex._2)).filter { case (y, x) =>
    unvisited.contains((y, x))
    y < 500 && x < 500 && shortestDistance.get((y, x)).map(!_._2).getOrElse(true)
  }
}

@annotation.tailrec
def dijkstra(distances: Coords => Int, unvisited: mutable.ArrayDeque[Coords], shortestDistance: mutable.Map[Coords, (Int, Boolean)]): mutable.Map[Coords, (Int, Boolean)] = {
  if(unvisited.size % 1000 == 0) {
    println(s"${unvisited.size}")
  }
  unvisitedVertexWithSmallestKnownDistance(unvisited, shortestDistance) match {
    case None => shortestDistance
    case Some((vertex, distanceVisited)) => {
      val distance = distanceVisited._1
      unvisitedNeighbourgs(unvisited, vertex, shortestDistance).foreach { case (y, x) =>
        val newDistance = distance + distances(y, x)
        val updatedDistance = shortestDistance.get((y, x)) match {
          case None => newDistance
          case Some((previous, _)) => if (newDistance < previous) newDistance else previous
        }
        shortestDistance.put((y, x), (updatedDistance, false))
      }
      unvisited -=  vertex
      shortestDistance.put(vertex, (distance, true))
      dijkstra(distances, unvisited, shortestDistance)
    }
  }
}


lazy val day15_risks = { println("uguu"); input(15).toList.map(l => l.map(_.toString.toInt).toList) }

def day15_1_distances(coords: Coords) = day15_risks(coords._1)(coords._2)

def day15_1 = {
  val side = day15_risks.length
  val unvisited = (0 until side).flatMap { y => (0 until side).map((y, _)) }.toList
  val result = dijkstra(day15_1_distances, unvisited.to(mutable.ArrayDeque), Map((0, 0) -> (0, false)).to(mutable.Map))
  result.get((side - 1, side - 1))
}

def day15_2_distances(coords: Coords) = {
  val (y, x) = coords
  val side = day15_risks.size
  val dist = day15_1_distances((y % side, x % side)) + y / side + x / side
  if (dist > 9) {
    dist % 10 + 1
  } else {
    dist
  }
}

def day15_2 = {
  val side = day15_risks.length * 5
  val unvisited = (0 until side).flatMap { y => (0 until side).map((y, _)) }.toList
  val result = dijkstra(day15_2_distances, unvisited.to(mutable.ArrayDeque), Map((0, 0) -> (0, false)).to( mutable.Map ))
  result.get((side - 1, side - 1))
}
