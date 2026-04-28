package kse

import cats.data.State
import kse.ReductionStrategy.*
import kse.Substitution.substitute
import kse.Term.*

import scala.language.postfixOps

// Основна суть - без входу в λ - ххахахахах уже ні :)
// бета-редукція відбувається одразу, аргумент НЕ обчислюється перед підстановкою.
// Всередину λ-абстракцій НЕ заходимо. - уже ні

object CallByName extends Strategy:

  def step(term: Term): Eval[Option[Term]] = reduce(term)

  private def reduce(term: Term): Eval[Option[Term]] = term match

    case App(Abs(x, body), arg) =>
      substitute(body, x, arg).map(Some(_))

    case App(func, arg) =>
      reduce(func).flatMap {
        case Some(f) => State.pure(Some(App(f, arg)))
        case None    => reduce(arg).map(_.map(App(func, _)))
      }

    case Abs(x, body) =>
      reduce(body).map(_.map(Abs(x, _)))

    case Var(_) =>
      State.pure(None)

//це фактично NormalOrder - різниця між ними зникла. 
//Єдине що лишилось концептуально відрізняє: в App(func, arg) CBN спочатку вичерпує func
//і тільки потім береться за arg, тоді як різниця з NormalOrder тут суто в порядку який співпадає