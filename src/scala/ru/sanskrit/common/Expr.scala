package ru.sanskrit.common

sealed trait Expr

case class Var(name: String)

enum Val extends Expr:
  case Var(x: Var)
  case Abs(x: Var, t: Expr)

enum Rhs:
  case Val(v: Val)
  case App(f: Val, x: Val)
  case Sum(a: Val, b: Val)
  case Mul(a: Val, b: Val)

case class Let(x: Var, t: Type, v: Rhs, b: Expr) extends Expr