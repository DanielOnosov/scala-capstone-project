package kse

import kse.Term.*
import kse.Generators.given
import kse.AlphaEquivalence.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object ParserSpecification extends Properties("Parser"):
  extension (t: Term)
    def show: String = t match
      case Var(n) => n
      case Abs(p, b) => s"(λ$p. ${b.show})"
      case App(f, a) => s"(${f.show} ${a.show})"

  property("Parses standalone variables") = propBoolean:
    Parser.parse("x") == Right(Var("x")) &&
      Parser.parse("  varName  ") == Right(Var("varName"))

  property("Parses abstractions with standard \\ and unicode λ") = propBoolean:
    val expected = Abs("x", Var("x"))
    Parser.parse("\\x. x") == Right(expected) &&
      Parser.parse("λx.x") == Right(expected) &&
      Parser.parse("  λx  .  x  ") == Right(expected)

  property("Parses nested abstractions") = propBoolean:
    // \x. \y. x
    val expected = Abs("x", Abs("y", Var("x")))
    Parser.parse("\\x. \\y. x") == Right(expected)

  property("Parses strictly left-associative applications") = propBoolean:
    // x y z -> (x y) z
    val expected = App(App(Var("x"), Var("y")), Var("z"))
    Parser.parse("x y z") == Right(expected)

  property("Parses parenthesized groupings correctly") = propBoolean:
    // x (y z)
    val expected = App(Var("x"), App(Var("y"), Var("z")))
    Parser.parse("x (y z)") == Right(expected)

  property("Parses the Omega combinator (λx. x x) (λx. x x)") = propBoolean:
    val expected = App(
      Abs("x", App(Var("x"), Var("x"))),
      Abs("x", App(Var("x"), Var("x")))
    )
    Parser.parse("(\\x. x x) (\\x. x x)") == Right(expected)

  property("Fails gracefully on invalid syntax") = propBoolean:
    Parser.parse("\\x x").isLeft && // missing dot
      Parser.parse("(x y").isLeft && // missing closing parenthesis
      Parser.parse("x y .").isLeft // invalid trailing dot

  property("Round-trip property: parse(show(term)) == Right(term)") = forAll: (term: Term) =>
    val serialized = term.show
    val parsed = Parser.parse(serialized)

    parsed == Right(term)

  property("Parse single variable") = propBoolean:
    Parser.parse("x") == Right(Var("x"))

  property("Parse abstraction (both \\ and λ syntaxes)") = propBoolean:
    val expected = Right(Abs("x", Var("x")))
    Parser.parse("\\x. x") == expected && Parser.parse("λx.x") == expected

  property("Parse application with left-associativity") = propBoolean:
    // "x y z" має парситися як "(x y) z", тобто App(App(x, y), z)
    val expected = Right(App(App(Var("x"), Var("y")), Var("z")))
    Parser.parse("x y z") == expected

  property("Parse application with parentheses") = propBoolean:
    // "x (y z)" має парситися як "x (y z)", тобто App(x, App(y, z))
    val expected = Right(App(Var("x"), App(Var("y"), Var("z"))))
    Parser.parse("x (y z)") == expected

  property("Syntax errors are handled gracefully (Left)") = propBoolean:
    Parser.parse("(x y").isLeft && // незакрита дужка
      Parser.parse("\\x y").isLeft && // пропущена крапка
      Parser.parse("x )").isLeft // зайва дужка

end ParserSpecification