val nums = List("1","2","3")

def isort(xs: List[Int]): List[Int] = xs match {
  case List() => List()
  case y :: ys => insert(y, isort(ys))
}

def insert(x: Int, xs: List[Int]): List[Int] = xs match {
  case List() => List(x)
  case y :: ys => if( x > y) y :: insert(x, ys.tail) else x :: xs
}

//7 , 3 , 9 , 2
//
//5
//2 :: 3 :: 9
//insert(7 , List(2,3,9))
