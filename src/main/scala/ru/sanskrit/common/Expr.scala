package ru.sanskrit.common

case class Name(name: String)

sealed trait Expr
case class Let(x: Name, t: Type, v: Expr, b: Expr) extends Expr

sealed trait Val extends Expr
case class Lit(x: Int) extends Val
case class LitArray(x: List[Int]) extends Val
case class Var(x: Name) extends Val
case class Abs(x: Name, t: Expr) extends Val
case class Pair(a: Var, b: Var) extends Val

case class App(f: Var, x: Var) extends Expr
case class Sum(a: Var, b: Var) extends Expr
case class Mul(a: Var, b: Var) extends Expr
case class Car(p: Var) extends Expr
case class Cdr(p: Var) extends Expr
