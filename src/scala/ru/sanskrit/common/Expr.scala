package ru.sanskrit.common

case class Name(name: String)

enum Rhs:
  case Val(v: Val)
  case App(f: Val, x: Val)
  case Sum(a: Val, b: Val)
  case Mul(a: Val, b: Val)

enum Expr:
  case Let(x: Name, t: Type, v: Rhs, b: Expr)

enum Val extends Expr:
  case Var(x: Name)
  case Abs(x: Name, t: Expr)
