package kse

import cats.data.State
import kse.ReductionStrategy.*
import kse.Substitution.substitute
import kse.Term.*

// Аргумент ОБОВ'ЯЗКОВО обчислюється до "значення" (λ або Var)
// перед тим, як відбудеться бета-редукція.
// Всередину λ-абстракцій НЕ заходимо.

object CallByValue extends Strategy:

  def step(term: Term): Eval[Option[Term]] = reduce(term)

  // Значення в CBV - λ-абстракція або змінна (не може бути далі зменшене)
  private def isValue(t: Term): Boolean = t match
    case Abs(_, _) => true
    case Var(_)    => true
    case _         => false // App(_, _) = false

  private def reduce(term: Term): Eval[Option[Term]] = term match

    // бета-редукція - тут це для випадку якщо аргумент вже є значенням
    case App(Abs(x, body), arg) if isValue(arg) =>
      substitute(body, x, arg).map(Some(_))

    // Колифункція - значення, але аргумент ще ні - то ми обчислюємо аргумент
    case App(func, arg) if isValue(func) =>
      reduce(arg).map(_.map(App(func, _)))

    // Функція ще не э значенням - тоды обчислюємо функцію першоюв порядку черги
    case App(func, arg) =>
      reduce(func).map(_.map(App(_, arg)))

    // λ і Var - вже значення, нічого не робимо
    case Abs(_, _) => State.pure(None)
    case Var(_)    => State.pure(None)
