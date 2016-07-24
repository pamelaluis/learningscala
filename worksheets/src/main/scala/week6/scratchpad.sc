Map("first" -> 1, "second" -> 2).foldLeft(Map("first" -> 9, "second" -> 2)) {
  case (map, (k, v)) => {
    map(k) - v match {
      case 0 => map - k
      case default => map updated(k, default)
    }
  }
}



  val x1 = List(('a', 3), ('d', 1), ('l', 1), ('r', 1))
  val y1 = List(('r', 1), ('a', 1))

  val x1Map = x1.toMap withDefaultValue 0
  (x1Map ++ (y1 map adjust)).filter { case (_, y) => y != 0 }.toList


def adjust(occurrence: (Char, Int)): (Char, Int) = {
  val (ch, occ) = occurrence
  ch -> (x1Map(ch) - occ)
}

List(
  List(),
  List(('a', 1)),
  List(('a', 2)),
  List(('b', 1)),
  List(('a', 1), ('b', 1)),
  List(('a', 2), ('b', 1)),
  List(('b', 2)),
  List(('a', 1), ('b', 2)),
  List(('a', 2), ('b', 2))
)

List.range(1, 3)
List(List(('b', 1)), List(('b', 2))) ::: List(List())

List(('b', 1)) :: List(List())

List(('a', 1)) ::: List(('b', 1))
val occurrences: List[(Char, Int)] = List(('a', 2), ('b', 2))
val occurrences2: List[(Char, Int)] = List(('b', 2))
val occurrences3: List[(Char, Int)] = List()

List(('b', 1)) drop 0

val r1 = for {
  (ch, n) <- occurrences2
  index <- 1 to n
} yield List((ch, index))

val r2 = for {
  (ch, n) <- occurrences3
  index <- 1 to n
} yield List((ch, index))

for {
  (ch, n) <- occurrences
  index <- 1 to n
  r <- r1
  if r forall (!_.equals(ch))
} yield (ch, index) :: r

List(List(('b', 1))).forall(_.forall(!_.equals('a')))




val x = ("daddy".toList.groupBy((element: Char) => element) map { case (ch, lst) => ch -> lst.length }).toList
x.toMap
"mummy".toList.sorted
List("mummy", "daddy").fold("")(_ + _)


