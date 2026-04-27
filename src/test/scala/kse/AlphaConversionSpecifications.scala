package kse

import kse.Term.*
import kse.Generators.given
import kse.AlphaEquivalence.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object AlphaConversionSpecifications extends Properties("Alpha Conversion (Alpha-Equivalence)"):

  property("Identity functions with different variable names are alpha-equivalent") = propBoolean:
    val id1 = Abs("x", Var("x")) // λx.x
    val id2 = Abs("y", Var("y")) // λy.y
    id1.alphaEq(id2)

  property("Constant functions with different bound variables are alpha-equivalent") = propBoolean:
    val c1 = Abs("x", Abs("y", Var("x"))) // λx.λy.x
    val c2 = Abs("a", Abs("b", Var("a"))) // λa.λb.a
    c1.alphaEq(c2)

  property("Free variables must match exactly to be alpha-equivalent") = propBoolean:
    val t1 = Abs("x", App(Var("x"), Var("y"))) // λx. x y
    val t2 = Abs("z", App(Var("z"), Var("y"))) // λz. z y
    val t3 = Abs("x", App(Var("x"), Var("w"))) // λx. x w

    t1.alphaEq(t2) && !t1.alphaEq(t3)

  property("Different structure means NOT alpha-equivalent") = propBoolean:
    val t1 = Abs("x", Var("x")) // λx.x
    val t2 = App(Var("x"), Var("x")) // x x
    !t1.alphaEq(t2)

  property("Alpha equivalence is reflexive (every term is alpha-equivalent to itself)") = forAll: (term: Term) =>
    term.alphaEq(term)

end AlphaConversionSpecifications