package ru.sanskrit.frontend

import ru.sanskrit.common.Type

object syntax:

  enum Expr:
    case Lit(x: Int)
    case Var(name: String)
    case App(f: Expr, x: Expr)
    case Lam(arg: Var, expr: Expr)

  case class Func(name: String, tp: Option[Type], body: Expr, args: (Expr.Var, Option[Type])*)
