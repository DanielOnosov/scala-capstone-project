package kse

import Fresh.*
import Term.*
import cats.data.State

object Substitution:

  extension (t: Term)

    def freeVars: Set[String] = t match
      case Var(n)    => Set(n)
      case Abs(p, b) => b.freeVars - p
      case App(f, a) => f.freeVars ++ a.freeVars

  def substitute(term: Term, x: String, n: Term): Fresh[Term] = term match

    case Var(name) if name == x =>
      State.pure(n) // 1. [N/x] x ≡ N

    case Var(_) =>
      State.pure(term) // 2. [N/x] a ≡ a for all atoms a ≠ x

    case App(f, a) =>
      for
        f2 <- substitute(f, x, n)
        a2 <- substitute(a, x, n)
      yield App(f2, a2) // 3. [N/x](P Q) ≡ ([N/x]P [N/x]Q)

    case Abs(p, body) if p == x =>
      State.pure(term) // 4. [N/x](λx.P) ≡ λx.P

    case Abs(p, body) if !freeVars(body).contains(x) =>
      State.pure(term) // 5. [N/x](λy.P) ≡ λy.P if x ∉ FV(P), y ≠ x

    case Abs(p, body) if !freeVars(n).contains(p) =>
      substitute(body, x, n).map(Abs(p, _)) // 6. [N/x](λy.P) ≡ λy.[N/x]P, if x ∊ FV(P) and  y ∉ FV(N), y ≠ x

    case Abs(p, body) =>
      val avoid = freeVars(n) ++ freeVars(body) + x + p
      for
        z       <- freshVar(avoid)
        renamed <- substitute(body, p, Var(z))
        res     <- substitute(renamed, x, n)
      yield Abs(z, res) // 7. [N/x](λy.P) ≡ λz.[N/x][z/y]P, if x ∊ FV(P) and  y ∊ FV(N), y ≠ x
