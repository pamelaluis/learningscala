import week4._

object exprs {
  def show(e: Expr): String = e match {
    case Number(n) => n.toString
    case Var(x) => x.toString
    case Sum(n1, n2) => show(n1) + " + " + show(n2)
    case Prod(n1, n2) =>
      def bracketIt(x: Expr): String = x match {
        case Sum(_,_) => "(" + show(x) + ")"
        case default => show(x)
      }
      bracketIt(n1) + " * " + bracketIt(n2)
  }
}
exprs.show(Sum(Prod(Number(2), Var("x")), Var("y")))

exprs.show(Prod(Sum(Number(2), Var("x")), Var("y")))


