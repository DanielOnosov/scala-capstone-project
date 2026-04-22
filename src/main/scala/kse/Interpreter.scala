package kse

import cats.data.State
import kse.Term.*
import kse.ReductionStrategy.*

object Interpreter:

  type Eval[A] = State[Int, A]

  def evaluate(
                term: Term,
                strategy: Strategy,
                maxSteps: Int
              ): Eval[Term] =

    def loop(current: Term, steps: Int): Eval[Term] =
      if steps >= maxSteps then
        State.pure(current)
      else
        strategy.step(current).flatMap {
          case Some(next) =>
            loop(next, steps + 1)

          case None =>
            State.pure(current)
        }

    loop(term, 0)