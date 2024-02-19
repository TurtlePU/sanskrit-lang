package ru.sanskrit.frontend

import cats.Applicative
import cats.syntax.applicative.*
import ru.sanskrit.common.Type

object syntax:
  enum Expr[F[_]]:
    case Lit(x: Int)
    case Var(name: String, `type`: F[Type])
    case App(f: Expr[F], x: Expr[F],`type`: F[Type])
    case Lam(arg: Var[F], expr: Expr[F], `type`: F[Type])

    def getType(using Applicative[F]): F[Type] = this match {
      case Lit(_)       => Type.Int.pure
      case Var(_, t)    => t
      case App(_, _, t) => t
      case Lam(_, _, t) => t
    }

  case class Func[F[_]](name: String, tp: F[Type], body: Expr[F], args: Expr.Var[F]*)
