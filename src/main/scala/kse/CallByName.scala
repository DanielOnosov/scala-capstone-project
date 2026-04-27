package kse

import cats.data.State
import kse.ReductionStrategy.*
import kse.Substitution.substitute
import kse.Term.*

// Основна суть - без входу в λ
// бета-редукція відбувається одразу, аргумент НЕ обчислюється перед підстановкою.
// Всередину λ-абстракцій НЕ заходимо.

object CallByName extends Strategy:

  def step(term: Term): Eval[Option[Term]] = reduce(term)

  private def reduce(term: Term): Eval[Option[Term]] = term match

    // бета-редукція - аргумент підставляємо "як є", без попереднього обчислення буквально "lazy" обчислення з підстановкою без обчислювання
    case App(Abs(x, body), arg) =>
      substitute(body, x, arg).map(Some(_))

    // App - тільки функція може бути зменшена; аргумент не чіпаємо
    case App(func, arg) =>
      reduce(func).map(_.map(App(_, arg)))

    // λ і Var - вже в нормальній формі для CBN
    case Abs(_, _) => State.pure(None)
    case Var(_)    => State.pure(None)
