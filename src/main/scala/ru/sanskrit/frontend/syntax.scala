package ru.sanskrit.frontend

import ru.sanskrit.common.Type

object syntax:

  enum Expr[F[_]]:
    case Lit(x: Int)
    case Var(name: String, `type`: F[Type])
    case App(f: Expr[F], x: Expr[F],`type`: F[Type])
    case Lam(arg: Var[F], expr: Expr[F], `type`: F[Type])

  case class Func(name: String, tp: Option[Type], body: Expr[Option], args: Expr.Var[Option]*)
