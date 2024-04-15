package ru.sanskrit.frontend

import cats.Applicative
import cats.parse.Caret
import cats.syntax.applicative.*
import ru.sanskrit.common.Type

object syntax:
  case class Position(begin: Caret, end: Caret)

  enum Expr[F[_]]:
    self =>
    case Lit(x: Int, position: Position)
    case Var(name: String, `type`: F[Type], position: Position)
    case App(f: Expr[F], x: Expr[F], `type`: F[Type], position: Position)
    case Lam(arg: Var[F], expr: Expr[F], `type`: F[Type], position: Position)
    case InfixOp(f: Var[F], x: Expr[F], y: Expr[F], `type`: F[Type], position: Position)

    def getType(using Applicative[F]): F[Type] = self match {
      case Lit(_, _)              => Type.Int.pure
      case Var(_, t, _)           => t
      case App(_, _, t, _)        => t
      case Lam(_, _, t, _)        => t
      case InfixOp(_, _, _, t, _) => t
    }

    def getPosition: Position = self match {
      case Lit(_, p)              => p
      case Var(_, _, p)           => p
      case App(_, _, _, p)        => p
      case Lam(_, _, _, p)        => p
      case InfixOp(_, _, _, _, p) => p
    }

    def updatePosition(begin: Caret, end: Caret): Expr[F] = self match {
      case Lit(x, _)              => Lit(x, Position(begin, end))
      case Var(name, t, _)        => Var(name, t, Position(begin, end))
      case App(f, x, t, _)        => App(f, x, t, Position(begin, end))
      case Lam(a, e, t, _)        => Lam(a, e, t, Position(begin, end))
      case InfixOp(f, x, y, t, _) => InfixOp(f, x, y, t, Position(begin, end))
    }

  case class Func[F[_]](name: String, tp: F[Type], body: Expr[F], args: Expr.Var[F]*)
