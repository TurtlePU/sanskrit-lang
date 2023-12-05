package ru.sanskrit.common

case class Name(name: String)

sealed trait Rhs
object Rhs:
  case class Val(v: Val) extends Rhs
  case class App(f: Val, x: Val) extends Rhs
  case class Sum(a: Val, b: Val) extends Rhs
  case class Mul(a: Val, b: Val) extends Rhs

sealed trait Expr
object Expr:
  case class Let(x: Name, t: Type, v: Rhs, b: Expr) extends Expr
  sealed trait Val extends Expr
  object Val:
    case class Var(x: Name) extends Val
    case class Abs(x: Name, t: Expr) extends Val
