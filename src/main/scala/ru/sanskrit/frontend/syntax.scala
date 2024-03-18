package ru.sanskrit.frontend

import cats.Applicative
import cats.parse.Caret
import cats.syntax.applicative.*
import ru.sanskrit.common.Type

object syntax:
  enum Expr[F[_]]:
    case Lit(x: Int, caret: Caret)
    case Var(name: String, `type`: F[Type], caret: Caret)
    case App(f: Expr[F], x: Expr[F], `type`: F[Type], caret: Caret)
    case Lam(arg: Var[F], expr: Expr[F], `type`: F[Type], caret: Caret)
    case InfixOp(f: Var[F], x: Expr[F], y: Expr[F], `type`: F[Type], caret: Caret)

    def getType(using Applicative[F]): F[Type] = this match {
      case Lit(_, _)              => Type.Int.pure
      case Var(_, t, _)           => t
      case App(_, _, t, _)        => t
      case Lam(_, _, t, _)        => t
      case InfixOp(_, _, _, t, _) => t
    }

    def getCaret: Caret = this match {
      case Lit(_, c)              => c
      case Var(_, _, c)           => c
      case App(_, _, _, c)        => c
      case Lam(_, _, _, c)        => c
      case InfixOp(_, _, _, _, c) => c
    }

  case class Func[F[_]](name: String, tp: F[Type], body: Expr[F], args: Expr.Var[F]*)
