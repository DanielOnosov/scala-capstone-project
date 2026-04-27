package kse

import cats.data.State
import kse.ReductionStrategy.*

case class ReductionResult(term: Term, stepsTaken: Int, isNormalForm: Boolean)

object Interpreter:

  def evaluateExtended(term: Term, strategy: Strategy, maxSteps: Int): Eval[ReductionResult] =
    def loop(current: Term, steps: Int): Eval[ReductionResult] =
      if steps >= maxSteps then State.pure(ReductionResult(current, steps, false))
      else
        strategy.step(current).flatMap {
          case Some(next) => loop(next, steps + 1)
          case None       => State.pure(ReductionResult(current, steps, true))
        }

    loop(term, 0)
