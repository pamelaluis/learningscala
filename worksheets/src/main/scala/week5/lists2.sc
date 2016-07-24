def isPrime(n: Int): Boolean = (2 until n) forall (x => n % x != 0)

(1 to 100) filter (n => isPrime(n))

def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => (y * y) :: squareList1(ys)
  }

def squareList2(xs: List[Int]): List[Int] =
  xs map (y => y * y)

val sl1 = squareList1(List(1,2,3,4,6))
val sl2 = squareList2(List(1,2,3,4,6))

def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (a1, a2) = xs span (s => s.equals(x))
    List(a1) ::: pack(a2)
}

val lst= List("a", "a", "a", "b", "c", "c", "a")
pack(lst)

encode(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]):List[(T,Int)] =
    pack(xs) map ( n => (n.head, n.length))

def concat[T](xs: List[T], ys: List[T]): List[T] =
  (xs foldRight  ys) (_ :: _)

//(List(1,2,3) foldLeft (List(4))) (_ :: _)

1 :: (2 :: (3 :: List(4)))
//(((1 :: 2) :: 3) :: List(4))

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_) :: _ )

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)( (_,y) => 1 + y )

val ints: List[Integer] = List(1, 2, 3)
mapFun[Integer, String](ints, "s" + _)
lengthFun(ints)
val Pattern = "([A-z\\s])".r

val testStrSeq = "Hello, World!'" filter (c => c match { case Pattern(_) => true case _ => false})
List(1,2,3) zip testStrSeq
testStrSeq zip List(1,2,3)





