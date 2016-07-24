import week3.{Cons, Nil, List}

def singleton[T](elem: T) = new Cons[T](elem, Nil)
singleton(1).head
singleton(true).head
singleton(null).head

def nth[T](n: Int, list: List[T]): T = {
  if (list.isEmpty)
      throw new IndexOutOfBoundsException
  else if(n == 0)
      list.head
  else nth(n -1, list.tail)
}

val list = new Cons(1, new Cons(2, new Cons(3, Nil)))
//nth(4, list)
nth(-1, list)

List(1,2)






