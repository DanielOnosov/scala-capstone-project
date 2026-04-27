package kse

import kse.ReductionStrategy.*
import kse.Term.*

// Додаємо extension метод для красивого виводу термів
extension (t: Term)

  def show: String = t match
    case Var(n)    => n
    case Abs(p, b) => s"(λ$p. ${b.show})"
    case App(f, a) => s"(${f.show} ${a.show})"

@main def runInterpreter(): Unit =
  println("==============================================")
  println("1. Базовий тест: (λx. x) y")
  println("==============================================")

  val identity = Abs("x", Var("x"))
  val term1    = App(identity, Var("y"))

  val (state1, result1) = Interpreter.evaluateExtended(term1, NormalOrder, 100).run(0).value
  println(s"Початковий терм: ${term1.show}")
  println(s"Результат:       ${result1.term.show}")
  println(s"Кроків:          ${result1.stepsTaken}, Нормальна форма: ${result1.isNormalForm}\n")

  println("==============================================")
  println("2. Тест перейменування змінних (Substitution)")
  println("==============================================")
  // (λx. λy. x y) y  => змінна 'y' в аргументі має конфліктувати з параметром 'y'
  val captureTerm       = App(Abs("x", Abs("y", App(Var("x"), Var("y")))), Var("y"))
  val (state2, result2) = Interpreter.evaluateExtended(captureTerm, NormalOrder, 100).run(0).value

  println(s"Початковий терм: ${captureTerm.show}")
  println(s"Результат:       ${result2.term.show}") // Повинно перейменувати y на _v0
  println(s"Використано змінних для перейменування: $state2\n")

  println("==============================================")
  println("3. Тест стратегій: Call-By-Name vs Call-By-Value")
  println("==============================================")
  // OMEGA: (λx. x x)(λx. x x) - безкінечний цикл
  val omega = App(Abs("x", App(Var("x"), Var("x"))), Abs("x", App(Var("x"), Var("x"))))
  // term3: (λz. y) OMEGA
  val term3 = App(Abs("z", Var("y")), omega)

  println(s"Терм: ${term3.show}")

  // Call-by-Name (ліниве обчислення - аргумент не обчислюється, якщо не потрібен)
  val (_, resCBN) = Interpreter.evaluateExtended(term3, CallByName, 50).run(0).value
  println(
    s"[Call-By-Name]  Результат: ${resCBN.term.show} | Кроків: ${resCBN.stepsTaken} | Нормальна форма: ${resCBN.isNormalForm}"
  )

  // Call-by-Value (жадібне обчислення - спочатку обчислюється аргумент OMEGA, що призводить до зависання)
  val (_, resCBV) = Interpreter.evaluateExtended(term3, CallByValue, 50).run(0).value
  println(
    s"[Call-By-Value] Результат: ${resCBV.term.show} | Кроків: ${resCBV.stepsTaken} | Нормальна форма: ${resCBV.isNormalForm}"
  )
  println("==============================================")
