package kse

import cats.data.State

object Fresh:

  type Fresh[A] = State[Int, A]

  def freshVar(avoid: Set[String]): Fresh[String] =
    State { i =>
      val name = LazyList.from(i).map(n => s"_v$n").dropWhile(avoid).head
      (i + 1, name)
    }
