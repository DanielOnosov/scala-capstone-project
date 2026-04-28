package kse

import kse.Term.*
import scala.annotation.tailrec

object Parser:

  enum Token:
    case Lam, Dot, LParen, RParen
    case Ident(name: String)

  /**
   * Лексичний аналізатор (лексер).
   * Перетворює вхідний текстовий рядок на послідовність токенів.
   *
   * Алгоритм:
   * Проходить по масиву символів за допомогою хвостової рекурсії.
   * Пробіли ігноруються. Спеціальні символи ('\', 'λ', '.', '(', ')')
   * перетворюються на відповідні токени. Буквено-цифрові послідовності
   * групуються у токени Ident (ідентифікатори).
   *
   * @param input вхідний рядок з виразом
   * @return Either з текстом помилки (Left) або списком токенів (Right)
   */
  def tokenize(input: String): Either[String, List[Token]] =
    @tailrec
    def loop(chars: List[Char], acc: List[Token]): Either[String, List[Token]] = chars match
      case Nil => Right(acc.reverse)
      case c :: rest if c.isWhitespace => loop(rest, acc)
      case '\\' :: rest                => loop(rest, Token.Lam :: acc)
      case 'λ' :: rest                 => loop(rest, Token.Lam :: acc)
      case '.' :: rest                 => loop(rest, Token.Dot :: acc)
      case '(' :: rest                 => loop(rest, Token.LParen :: acc)
      case ')' :: rest                 => loop(rest, Token.RParen :: acc)

      case c :: _ if c.isLetterOrDigit =>
        val (identChars, rest) = chars.span(_.isLetterOrDigit)
        loop(rest, Token.Ident(identChars.mkString) :: acc)

      case invalid :: _ =>
        Left(s"Lexer error: Invalid character '$invalid'")

    loop(input.toList, Nil)

  /**
   * Лексичний аналізатор (лексер).
   * Перетворює вхідний текстовий рядок на послідовність токенів.
   *
   * Алгоритм:
   * Проходить по масиву символів за допомогою хвостової рекурсії.
   * Пробіли ігноруються. Спеціальні символи ('\', 'λ', '.', '(', ')')
   * перетворюються на відповідні токени. Буквено-цифрові послідовності
   * групуються у токени Ident (ідентифікатори).
   *
   * @param input вхідний рядок з виразом
   * @return Either з текстом помилки (Left) або списком токенів (Right)
   */
  def parse(input: String): Either[String, Term] =
    for
      tokens             <- tokenize(input)
      (term, leftover)   <- parseTerm(tokens)
      result             <- if leftover.isEmpty then Right(term)
      else Left(s"Syntax error. Unexpected trailing tokens: $leftover")
    yield result

  /**
   * Аналізує вираз (Term) на верхньому рівні.
   *
   * Логіка розбору:
   * 1. Якщо послідовність починається з токенів `Lam -> Ident -> Dot`,
   * це абстракція (наприклад, `λx.y`). Функція рекурсивно викликає
   * себе для розбору тіла абстракції.
   * 2. В іншому випадку вираз розглядається як застосування (Application)
   * або окремий атом (змінна чи вираз у дужках).
   */
  private def parseTerm(tokens: List[Token]): Either[String, (Term, List[Token])] = tokens match
    case Token.Lam :: Token.Ident(name) :: Token.Dot :: rest =>
      for
        (body, leftover) <- parseTerm(rest)
      yield (Abs(name, body), leftover)

    case _ =>
      for
        (firstAtom, rest) <- parseAtom(tokens)
        result            <- parseApp(firstAtom, rest)
      yield result

  /**
   * Аналізує атомарний вираз — базову неподільну одиницю синтаксису.
   * Атомом може бути:
   * - Ідентифікатор (змінна).
   * - Інший вираз (Term), який повністю взятий у круглі дужки.
   */
  private def parseAtom(tokens: List[Token]): Either[String, (Term, List[Token])] = tokens match
    case Token.Ident(name) :: rest =>
      Right((Var(name), rest))

    case Token.LParen :: rest =>
      for
        (term, restAfterTerm) <- parseTerm(rest)
        tail <- restAfterTerm match
          case Token.RParen :: t => Right(t)
          case _                 => Left("Syntax error: Missing closing parenthesis ')'")
      yield (term, tail)

    case Nil =>
      Left("Syntax error: Unexpected end of input")

    case head :: _ =>
      Left(s"Syntax error: Unexpected token $head")


  /**
   * Аналізує застосування функцій (Application).
   *
   * У лямбда-численні застосування є лівоасоціативним. Тобто вираз `x y z`
   * інтерпретується як `((x y) z)`.
   *
   * Функція використовує хвостову рекурсію: вона зчитує наступний атом і
   * обгортає поточний акумулятор у новий вузол `App(acc, nextAtom)`, доки
   * не зустріне кінець послідовності або закриваючу дужку (яка сигналізує
   * про вихід із поточного контексту парсингу).
   *
   * @param acc    поточне розібране піддерево (акумулятор)
   * @param tokens залишкові токени для аналізу
   */
  @tailrec
  private def parseApp(acc: Term, tokens: List[Token]): Either[String, (Term, List[Token])] = tokens match
    case Nil => Right((acc, Nil))
    case Token.RParen :: _ => Right((acc, tokens))
    case _ =>
      parseAtom(tokens) match
        case Right((nextAtom, rest)) => parseApp(App(acc, nextAtom), rest)
        case Left(err) => Left(err)

end Parser
