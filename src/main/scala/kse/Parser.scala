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
      case "λ"  => Token.Lam
      case "."  => Token.Dot
      case "("  => Token.LParen
      case ")"  => Token.RParen
      case name => Token.Ident(name)
    }

  def parse(input: String): Either[String, Term] =
    try
      val tokens           = tokenize(input)
      val (term, leftover) = parseTerm(tokens)

      if leftover.isEmpty then Right(term)
      else Left(s"Syntax error. Unexpected trailing tokens: $leftover")
    catch case e: Exception => Left(e.getMessage)

  private def parseTerm(tokens: List[Token]): (Term, List[Token]) = tokens match
    case Token.Lam :: Token.Ident(name) :: Token.Dot :: rest =>
      val (body, leftover) = parseTerm(rest)
      (Abs(name, body), leftover)

    case _ =>
      val (firstAtom, rest) = parseAtom(tokens)
      parseApp(firstAtom, rest)

  private def parseAtom(tokens: List[Token]): (Term, List[Token]) = tokens match
    case Token.Ident(name) :: rest =>
      (Var(name), rest)

    case Token.LParen :: rest =>
      val (term, restAfterTerm) = parseTerm(rest)
      restAfterTerm match
        case Token.RParen :: tail => (term, tail)
        case _                    => throw new Exception("Missing closing parenthesis ')'")

    case Nil =>
      throw new Exception("Unexpected end of input")

    case head :: _ =>
      throw new Exception(s"Unexpected token: $head")

  @tailrec
  private def parseApp(acc: Term, tokens: List[Token]): (Term, List[Token]) = tokens match
    case Nil               => (acc, Nil)
    case Token.RParen :: _ => (acc, tokens)
    case _ =>
      val (nextAtom, rest) = parseAtom(tokens)
      parseApp(App(acc, nextAtom), rest)

end Parser
