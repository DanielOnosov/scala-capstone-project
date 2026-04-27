package kse

import kse.Term.*

object AlphaEquivalence:

  extension (t1: Term)
    def alphaEq(t2: Term): Boolean =
      def loop(term1: Term, term2: Term, env1: Map[String, Int], env2: Map[String, Int], depth: Int): Boolean =
        (term1, term2) match
          // Якщо це змінні, вони або обидві зв'язані на однаковій глибині, або обидві вільні з однаковим ім'ям
          case (Var(x), Var(y)) =>
            (env1.get(x), env2.get(y)) match
              case (Some(d1), Some(d2)) => d1 == d2 // Обидві зв'язані
              case (None, None)         => x == y   // Обидві вільні
              case _                    => false    // Одна зв'язана, інша вільна

          case (App(f1, a1), App(f2, a2)) =>
            loop(f1, f2, env1, env2, depth) && loop(a1, a2, env1, env2, depth)

          case (Abs(x, b1), Abs(y, b2)) =>
            loop(b1, b2, env1 + (x -> depth), env2 + (y -> depth), depth + 1)

          case _ => false

      loop(t1, t2, Map.empty, Map.empty, 0)

end AlphaEquivalence