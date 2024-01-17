package ru.sanskrit.common

case class Name(name: String)

sealed trait Rhs
object Rhs:
  case class Val(v: Expr.Val) extends Rhs
  case class App(f: Rhs, x: Rhs) extends Rhs
  case class Sum(a: Rhs, b: Rhs) extends Rhs
  case class Mul(a: Rhs, b: Rhs) extends Rhs
  case class Abs(x: Name, t: Rhs) extends Rhs

sealed trait Expr
object Expr:
  case class Let(x: Name, t: Type, v: Rhs, b: Expr) extends Expr
  sealed trait Val extends Expr
  object Val:
    case class Lit(x: Int) extends Val
    case class Var(x: Name) extends Val
