def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val rowCol = (row - 1) to 0 by -1 zip queens
  rowCol forall {
    case (r, c) => col != c && math.abs(col - c) != row -r
  }
}


def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] =
    if ( k == 0 ) Set(List())
    else
      for {
        queens <- placeQueens(k - 1)
        col <- 0 until n
        if(isSafe(col, queens))
      } yield col :: queens
  placeQueens(n)
}

queens(4)

for {
  n <- Set(List())
  col <- 0 until 1
} yield(20)



