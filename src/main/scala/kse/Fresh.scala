package kse

import cats.data.State

object Fresh:

  type Fresh[A] = State[Int, A]

  def freshVar(avoid: Set[String]): Fresh[String] =
    State { i =>
      val (name, j) = LazyList
        .from(i)
        .map(n => (s"_v$n", n))
        .dropWhile((s, _) => avoid(s))
        .head
      (j + 1, name)
    }
