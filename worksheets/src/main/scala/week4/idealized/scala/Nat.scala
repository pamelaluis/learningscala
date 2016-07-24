package week4.idealized.scala

/**
  * Created by David on 6/30/2016.
  */
abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat = new Succ(this)
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero: Boolean = true
  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = if(that.isZero) this else throw new UnsupportedOperationException("Zero.-")

  override def predecessor: Nat = throw new UnsupportedOperationException("Zero.-")

  override def toString: String = "0"
}

class Succ(n: Nat) extends Nat {
  override def isZero: Boolean = false

  override def +(that: Nat): Nat = new Succ(n + that)

  override def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor

  override def predecessor: Nat = n

}
