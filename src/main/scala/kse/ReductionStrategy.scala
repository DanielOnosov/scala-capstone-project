package kse

import cats.data.State
import kse.Substitution.*
import kse.Term.*

object ReductionStrategy:

  // ─────────────────────────────────────────────
  // EFFECT TYPE
  // State = fresh variable supply + step counter (you can reuse same Int)
  // ─────────────────────────────────────────────
  type Eval[A] = State[Int, A]

  // ─────────────────────────────────────────────
  // STRATEGY ABSTRACTION
  // ─────────────────────────────────────────────
  trait Strategy:
    def step(t: Term): Eval[Option[Term]]

  // ─────────────────────────────────────────────
  // NORMAL ORDER (LEFTMOST-OUTERMOST)
  // ─────────────────────────────────────────────
  object NormalOrder extends Strategy:

    def step(term: Term): Eval[Option[Term]] =
      reduce(term)

    // Core reducer: performs exactly ONE normal-order step
    private def reduce(term: Term): Eval[Option[Term]] = term match

      // ─────────────────────────────────────
      // 1. β-REDUCTION (OUTERMOST FIRST)
      // (λx. P) N  →  [N/x]P
      // ─────────────────────────────────────
      case App(Abs(x, body), arg) =>
        substitute(body, x, arg).map(Some(_))

      // ─────────────────────────────────────
      // 2. LEFTMOST: try function first
      // ─────────────────────────────────────
      case App(func, arg) =>
        reduce(func).flatMap {
          case Some(fReduced) =>
            State.pure(Some(App(fReduced, arg)))

          case None =>
            reduce(arg).map {
              case Some(aReduced) => Some(App(func, aReduced))
              case None           => None
            }
        }

      // ─────────────────────────────────────
      // 3. ABSTRACTION: reduce inside body
      // ─────────────────────────────────────
      case Abs(x, body) =>
        reduce(body).map(_.map(Abs(x, _)))

      // ─────────────────────────────────────
      // 4. VARIABLE: already normal form
      // ─────────────────────────────────────
      case Var(_) =>
        State.pure(None)
