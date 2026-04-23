import kse.*
import kse.Term.*
import kse.ReductionStrategy.*

@main def runInterpreter(): Unit =
  // Приклад: (λx. x) y
  val identity = Abs("x", Var("x"))
  val term = App(identity, Var("y"))

  val (finalState, result) = Interpreter.evaluateExtended(term, NormalOrder, 100).run(0).value
  println(s"Початковий терм: $term")
  println(s"Результат редукції: $result")