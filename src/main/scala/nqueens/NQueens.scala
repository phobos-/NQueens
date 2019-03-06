package nqueens

import scala.annotation.tailrec

object NQueens extends App {

  case class Candidate(cols: List[Int] = Nil, size: Int = 8) {
    def clashes: Boolean = {
      cols.toSet.size != cols.size ||
      cols.zipWithIndex.combinations(2).exists { case List((r1, c1), (r2, c2)) => math.abs(r1 - r2) == math.abs(c1 - c2) }
    }

    def children: List[Candidate] = for {
      c <- (0 until size).toList.diff(cols)
    } yield Candidate(c :: cols, size)

    override def toString: String = {
      "\n" + " _" * size +
      (for (c <- cols) yield "|_" * c + "|Q|" + "_|" * (size - 1 - c)).mkString("\n", "\n", "\n")
    }
  }

  @tailrec
  final def solve(candidates: List[Candidate] = List(Candidate()), soFar: List[Candidate] = List()): List[Candidate] = candidates match {
    case candidate :: tail =>
      if (candidate.clashes) solve(tail, soFar)
      else if (candidate.cols.size == candidate.size) solve(tail, candidate :: soFar)
      else solve(candidate.children ::: tail, soFar)
    case Nil => soFar
  }

  println(solve())
}