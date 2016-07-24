import week4.{A, B}

import scala.collection.immutable.List

val x: List[A] = new A::Nil
val y: List[B] = new B::Nil
val z: List[A] = new B::Nil:::y

val n: A = z.tail.head

//val a: Array[B] = Array(new B)
//val b: Array[A] = a
//b(0) = new C
//val s: B = a(0)

type X = A => B
type Y = B => A

val x1: List[X] = Nil
val x2: List[Y] = Nil
val x3: List[Y] = x1:::x2
