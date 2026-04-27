package kse

import kse.Term.*
import org.scalacheck.*
import org.scalacheck.{Arbitrary, Gen}

object Generators:

  // 1. Генератор імен змінних (використовуємо обмежений набір, щоб збільшити шанс колізій і перевірити Substitution)
  val varNameGen: Gen[String] = Gen.oneOf("x", "y", "z", "a", "b", "c")

  // 2. Базовий генератор змінних
  val varGen: Gen[Term] = varNameGen.map(Var.apply)

  // 3. Рекурсивний генератор дерев термів з контролем глибини
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

  // 4. Оголошуємо Arbitrary (стандартний тайпклас ScalaCheck) для Term
  given Arbitrary[Term] = Arbitrary(Gen.sized(size => termGen(math.min(size, 7))))
