package week4.idealized.scala

/**
  * Created by David on 6/30/2016.
  */
abstract class Boolean1 {
  def ifThenElse[T](t: => T, e: => T): T

  def && (x: => Boolean1): Boolean1 = ifThenElse(x, False)
  def || (x: => Boolean1): Boolean1 = ifThenElse(True, x)
  def unary_! : Boolean1 = ifThenElse(False, True)

  def == (x: => Boolean1): Boolean1 = ifThenElse(x,unary_!)
  def != (x: => Boolean1): Boolean1 = ifThenElse(unary_!,x)

  def < (x: => Boolean1): Boolean1 = ifThenElse(False, x)

}

object False extends Boolean1 {
  override def ifThenElse[T](t: => T, e: => T): T = e
}

object True extends Boolean1 {
  override def ifThenElse[T](t: => T, e: => T): T = t
}