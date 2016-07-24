class Poly(terms0: Map[Int, Double] ) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  def ++ (other: Poly): Poly = new Poly(terms ++ (other.terms map adjust1))
  def + (other: Poly): Poly =
    new Poly((other.terms foldLeft terms) (addTerm))
  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff) = term
    terms + (exp -> (terms(exp) + coeff))
  }
  def adjust(term: (Int,Double)): (Int, Double) = {
    val (exp, coeff) = term
    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => term
     }
  }
  def adjust1(term: (Int,Double)): (Int, Double) = {
    val (exp, coeff) = term
    exp -> (coeff + terms(exp))
  }
  override def toString =
    (for( (exp, coeff) <- terms.toList.sorted.reverse ) yield coeff + "^" + exp) mkString " + "
}

Map(1 -> 2.0, 5 -> 4.0, 3 -> 6.2 ).groupBy(_._1)
val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2 )
val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
p1 ++ p2