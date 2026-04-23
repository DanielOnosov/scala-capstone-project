package kse

import cats.data.State
import kse.Term.*
import kse.Substitution.substitute
import kse.ReductionStrategy.*

// CALL-BY-NAME - leftmost-outermost, без входу в λ
//
// β-редукція відбувається одразу, аргумент НЕ обчислюється перед підстановкою.
// Всередину λ-абстракцій НЕ заходимо.

object CallByName extends Strategy:

  def step(term: Term): Eval[Option[Term]] = reduce(term)

  private def reduce(term: Term): Eval[Option[Term]] = term match

    // β-редукція - аргумент підставляємо "як є", без попереднього обчислення
    case App(Abs(x, body), arg) =>
      substitute(body, x, arg).map(Some(_))

    // App - тільки функція може бути зменшена; аргумент не чіпаємо
    case App(func, arg) =>
      reduce(func).map(_.map(App(_, arg)))

    // λ і Var - вже в нормальній формі для CBN
    case Abs(_, _) => State.pure(None)
    case Var(_)    => State.pure(None)
