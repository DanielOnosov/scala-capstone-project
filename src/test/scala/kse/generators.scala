package kse

import kse.Term.*
import org.scalacheck.*
import org.scalacheck.{Arbitrary, Gen}

object Generators:

  val varNameGen: Gen[String] = Gen.oneOf("x", "y", "z", "a", "b", "c")

  val varGen: Gen[Term] = varNameGen.map(Var.apply)

  // Рекурсивний генератор дерев термів з контролем глибини
  def termGen(maxDepth: Int): Gen[Term] =
    if maxDepth <= 0 then varGen
    else
      Gen.frequency(
        (3, varGen),
        (
          2,
          for
            v    <- varNameGen
            body <- Gen.lzy(termGen(maxDepth - 1))
          yield Abs(v, body),
        ),
        (
          2,
          for
            f <- Gen.lzy(termGen(maxDepth - 1))
            a <- Gen.lzy(termGen(maxDepth - 1))
          yield App(f, a),
        ),
      )

  given Arbitrary[Term] = Arbitrary(Gen.sized(size => termGen(math.min(size, 7))))
