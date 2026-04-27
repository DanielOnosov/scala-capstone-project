package kse

import kse.Term.*
import scala.annotation.tailrec

object Parser:

  enum Token:
    case Lam, Dot, LParen, RParen
    case Ident(name: String)

  def tokenize(input: String): List[Token] =
    val spaced = input
      .replace("\\", " λ ")
      .replace("λ", " λ ")
      .replace(".", " . ")
      .replace("(", " ( ")
      .replace(")", " ) ")

    spaced.split("\\s+").toList.filter(_.nonEmpty).map {
      case "λ" => Token.Lam
      case "." => Token.Dot
      case "(" => Token.LParen
      case ")" => Token.RParen
      case name => Token.Ident(name)
    }

  def parse(input: String): Either[String, Term] =
    val tokens = tokenize(input)
    parseTerm(tokens) match
      case Right((term, leftover)) =>
        if leftover.isEmpty then Right(term)
        else Left(s"Syntax error. Unexpected trailing tokens: $leftover")
      case Left(error) => Left(error)

  private def parseTerm(tokens: List[Token]): Either[String, (Term, List[Token])] = tokens match
    case Token.Lam :: Token.Ident(name) :: Token.Dot :: rest =>
      parseTerm(rest) match
        case Right((body, leftover)) => Right((Abs(name, body), leftover))
        case Left(error) => Left(error) // Propagate error

    case _ =>
      parseAtom(tokens) match
        case Right((firstAtom, rest)) => parseApp(firstAtom, rest)
        case Left(error) => Left(error)

  private def parseAtom(tokens: List[Token]): Either[String, (Term, List[Token])] = tokens match
    case Token.Ident(name) :: rest =>
      Right((Var(name), rest))

    case Token.LParen :: rest =>
      parseTerm(rest) match
        case Right((term, Token.RParen :: tail)) => Right((term, tail))
        case Right((_, leftover)) => Left("Missing closing parenthesis ')'")
        case Left(error) => Left(error)

    case Nil =>
      Left("Unexpected end of input")

    case head :: _ =>
      Left(s"Unexpected token: $head")

  @tailrec
  private def parseApp(acc: Term, tokens: List[Token]): Either[String, (Term, List[Token])] = tokens match
    case Nil => Right((acc, Nil))
    case Token.RParen :: _ => Right((acc, tokens))
    case _ =>
      parseAtom(tokens) match
        case Right((nextAtom, rest)) => parseApp(App(acc, nextAtom), rest)
        case Left(error) => Left(error)

end Parser