package kse

sealed trait Term

object Term:

  final case class Var(name: String) extends Term
  final case class Abs(param: String, body: Term) extends Term
  final case class App(func: Term, arg: Term) extends Term