package kse

import kse.Generators.given
import kse.ReductionStrategy.*
import kse.Substitution.*
import kse.Term.*
import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean}
import org.scalacheck.Properties

object GeneralInterpreterSpecification extends Properties("Lambda Calculus Interpreter"):

  include(SubstitutionSpecification)
  include(ReductionSpecification)
  include(StrategySpecification)
  include(FreeVariablesSpecification)
  include(BasicSubstitutionLawsSpecification)
  include(InterpreterLimitsSpecification)
  include(SemanticReductionSpecification)
  include(ParserSpecification)
  include(AlphaConversionSpecifications)

end GeneralInterpreterSpecification

object SubstitutionSpecification extends Properties("Substitution laws"):

  property("Substitution correctly bounds free variables") = forAll: (m: Term, n: Term) =>
    val x               = m.freeVars.headOption.getOrElse("x")
    val substitutedTerm = Substitution.substitute(m, x, n).runA(0).value

    val fvSubstituted  = substitutedTerm.freeVars
    val expectedSubset = (m.freeVars - x) ++ n.freeVars

    // FV([N/x]M) ⊆ (FV(M) \ {x}) ∪ FV(N)
    fvSubstituted.subsetOf(expectedSubset)

  property("Substituting a variable with itself leaves free variables unchanged") = forAll: (term: Term) =>
    val x               = term.freeVars.headOption.getOrElse("x")
    val substitutedTerm = Substitution.substitute(term, x, Var(x)).runA(0).value

    substitutedTerm.freeVars == term.freeVars

end SubstitutionSpecification

object ReductionSpecification extends Properties("Reduction laws"):

  val MaxStepsLimit = 30

  property("Normal Order evaluation is deterministic") = forAll: (term: Term) =>
    val run1 = Interpreter.run(term, NormalOrder, MaxStepsLimit)
    val run2 = Interpreter.run(term, NormalOrder, MaxStepsLimit)

    run1 == run2

  property("Reduction does not introduce new free variables") = forAll: (term: Term) =>
    val fvBefore = term.freeVars
    val result   = Interpreter.run(term, NormalOrder, MaxStepsLimit)
    val fvAfter  = result.term.freeVars

    fvAfter.subsetOf(fvBefore)

  property("A term in Normal Form cannot be reduced further") = forAll: (term: Term) =>
    val firstPass = Interpreter.run(term, NormalOrder, MaxStepsLimit)

    if firstPass.isNormalForm then
      val secondPass = Interpreter.run(firstPass.term, NormalOrder, MaxStepsLimit)
      secondPass.stepsTaken == 0 && secondPass.term == firstPass.term
    else true // Пропускаємо перевірку, якщо нормальна форма ще не досягнута

end ReductionSpecification

object StrategySpecification extends Properties("Evaluation Strategy specifics"):

  // (λx. x x)(λx. x x)
  val omega: Term = App(Abs("x", App(Var("x"), Var("x"))), Abs("x", App(Var("x"), Var("x"))))

  // (λz. y)
  val constF: Term = Abs("z", Var("y"))

  // (λz. y) Ω
  val testTerm: Term = App(constF, omega)

  property("Call-By-Value always evaluates arguments (loops on Omega)") = propBoolean:
    val cbvResult = Interpreter.run(testTerm, CallByValue, 100)

    !cbvResult.isNormalForm && cbvResult.stepsTaken == 100

  property("Call-By-Name avoids evaluating unused arguments (terminates on Omega)") = propBoolean:
    val cbnResult = Interpreter.run(testTerm, CallByName, 100)

    cbnResult.isNormalForm && cbnResult.term == Var("y")

end StrategySpecification

object FreeVariablesSpecification extends Properties("Free Variables laws"):

  property("FV of a variable is just the variable itself") = forAll: (name: String) =>
    Var(name).freeVars == Set(name)

  property("FV of an abstraction removes the bound variable") = forAll: (param: String, body: Term) =>
    Abs(param, body).freeVars == (body.freeVars - param)

  property("FV of an application is the union of FVs of its parts") = forAll: (f: Term, a: Term) =>
    App(f, a).freeVars == (f.freeVars ++ a.freeVars)

end FreeVariablesSpecification

object BasicSubstitutionLawsSpecification extends Properties("Basic Substitution axioms"):

  property("[N/x] x == N (Substituting a variable with N yields N)") = forAll: (n: Term) =>
    val result = Substitution.substitute(Var("x"), "x", n).runA(0).value
    result == n

  property("[N/x] y == y (Substituting for a different variable does nothing)") = forAll: (n: Term) =>
    val result = Substitution.substitute(Var("y"), "x", n).runA(0).value
    result == Var("y")

  property("[N/x] M == M if x is not free in M") = forAll: (m: Term, n: Term) =>
    // Гарантуємо, що x не є вільною змінною в M
    val unusedVar = "unused_xyz"
    if !m.freeVars.contains(unusedVar) then
      val result = Substitution.substitute(m, unusedVar, n).runA(0).value
      result == m
    else true // Якщо раптом згенерувалося з "unused_xyz" (дуже малий шанс), ігноруємо

end BasicSubstitutionLawsSpecification

object InterpreterLimitsSpecification extends Properties("Interpreter Limits laws"):

  property("If maxSteps is 0, evaluation halts immediately and is not in Normal Form") = forAll: (term: Term) =>
    val result = Interpreter.run(term, NormalOrder, 0)

    result.stepsTaken == 0 && !result.isNormalForm

  property("A single Variable evaluates in 0 steps and is already in Normal Form") = forAll: (name: String) =>
    val result = Interpreter.run(Var(name), NormalOrder, 10)

    result.stepsTaken == 0 && result.isNormalForm

end InterpreterLimitsSpecification

object SemanticReductionSpecification extends Properties("Semantic Reduction laws"):

  // Функція ідентичності: λx. x
  val idFunc: Term = Abs("x", Var("x"))

  // Константна функція: λx. y
  val constFunc: Term = Abs("x", Var("y"))

  property("Applying Identity function to any argument N yields N in EXACTLY 1 step (CBN)") = forAll: (arg: Term) =>
    val term = App(idFunc, arg)

    // Перевіряємо рівно ОДИН крок стратегії напряму, без циклу run
    val singleStepResult = CallByName.step(term).runA(0).value

    // Результатом одного кроку має бути Some(arg)
    singleStepResult == Some(arg)

  property("Applying Constant function to any argument yields the constant in EXACTLY 1 step (CBN)") = forAll:
    (arg: Term) =>
      val term = App(constFunc, arg)

      val singleStepResult = CallByName.step(term).runA(0).value

      singleStepResult == Some(Var("y"))

  property("Normal Order reduces under lambda abstractions") = propBoolean:
    // (\x. (\y. y) z) -> (\x. z)
    val nestedIdentity = App(Abs("y", Var("y")), Var("z"))
    val term           = Abs("x", nestedIdentity)

    val result = Interpreter.run(term, NormalOrder, 10)

    result.term == Abs("x", Var("z"))

  property("Call By Value and Call By Name DO NOT reduce under lambda abstractions") = propBoolean:
    // (\x. (\y. y) z)
    val nestedIdentity = App(Abs("y", Var("y")), Var("z"))
    val term           = Abs("x", nestedIdentity)

    val cbnResult = Interpreter.run(term, CallByName, 10)
    val cbvResult = Interpreter.run(term, CallByValue, 10)

    cbnResult.term == term && cbnResult.stepsTaken == 0 &&
    cbvResult.term == term && cbvResult.stepsTaken == 0

end SemanticReductionSpecification
