val r1: Map[Char,Int] = Map('x' -> 1, 'y' -> 3)
r1.contains('z')
r1('x')
(r1 ++ Map('x' -> 2)).values

val x:List[(Char,Int)] = List(('c',9),('b',1),('a',2))
x.sortBy(t => t._2).map(r => r._2)

val ints: List[Int] = List(1, 2, 3, 4)
List((1,11),(2,22)).toMap