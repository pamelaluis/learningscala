import math.Ordering
def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  val n = xs.length / 2
  if(n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge(msort(fst)(lt), msort(snd)(lt))(lt)
  }
}

def msort1[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if(n == 0) xs
  else {
    val (fst, snd) = xs splitAt n
    merge2(msort1(fst), msort1(snd))
  }
}

def merge1(xs: List[Int], ys: List[Int]): List[Int] = {
  xs match {
    case Nil =>
      ys
    case x :: xs1 =>
      ys match {
        case Nil =>
          xs
        case y :: ys1 =>
          if(x<y) x :: merge1(xs1, ys)
          else y :: merge1(xs, ys1)
      }
  }
}

def merge[T](xs: List[T], ys: List[T])(lt: (T, T) => Boolean): List[T] =
  (xs, ys) match {
    case (Nil, ys1) => ys1
    case (xs1, Nil) => xs1
    case (x :: xs1 , y :: ys1) =>
      if (lt(x, y)) x :: merge(xs1, ys)(lt)
      else y :: merge(xs, ys1)(lt)
  }

def merge2[T](xs: List[T], ys: List[T])(implicit ord: Ordering[T]): List[T] =
  (xs, ys) match {
    case (Nil, ys1) => ys1
    case (xs1, Nil) => xs1
    case (x :: xs1 , y :: ys1) =>
      if (ord.lt(x, y)) x :: merge2(xs1, ys)
      else y :: merge2(xs, ys1)
  }

val fruits = List("apple", "pineapple", "orange", "banana")

msort(fruits)((x,y) => x.compareTo(y) <0)