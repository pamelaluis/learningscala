package week3

import java.util.NoSuchElementException

/**
  * Created by David on 6/29/2016.
  */
trait List[+T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
  def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}
class Cons[T](val head: T, val tail: List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

object Nil extends List[Nothing]{
  override def isEmpty: Boolean = true

  override def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def head: Nothing = throw new NoSuchElementException("Nil.head")
}

object List{
  def apply[T](): List[T] = Nil
  def apply[T](x: T): List[T] = new Cons[T](x, Nil)
  def apply[T](x: T, y: T): List[T] = new Cons[T](y,new Cons[T](x,Nil))
}

object test {
  val x: List[String] = Nil

}
