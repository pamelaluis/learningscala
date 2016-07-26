def init[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("init of empty list")
  case List(x) => Nil
  case y :: ys => y :: init(ys)
}

1 :: List(2)
List(2).::(1) ++ List(3)

def removeAt[T](n: Int, xs: List[T]): List[T] = {
  def iterate(i: Int, x: List[T]): List[T] = {
    x match {
      case List() => throw new IndexOutOfBoundsException
      case List(_) => if (i == n) List() else throw new IndexOutOfBoundsException
      case y :: ys => if (i == n) ys else y :: iterate(i + 1, ys)
    }
  }
  iterate(0, xs)
}

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)

def removeAt1[T](n: Int, xs: List[T]): List[T] = (xs take n) ::: (xs drop n+1)

removeAt1(2, List('a', 'b', 'c', 'd'))


def flatten(xs: List[Any]): List[Any] = xs match {
  case Nil => Nil
  case (h:List[Any])::tail => flatten(h):::flatten(tail)
  case h::tail => h::flatten(tail)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))

