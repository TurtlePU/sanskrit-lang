package ru.sanskrit.frontend

import ru.sanskrit.frontend.syntax.Expr
import ru.sanskrit.common.Type

object typecheck:
  def inferType(e: Expr, ctx: Map[String, Type]): Option[Type] = e match {
    case Expr.Lit(_)    => Some(Type.Int)
    case Expr.Var(name) => ctx.get(name)
    case Expr.App(f, x) => inferType(f, ctx).flatMap {
      case Type.Func(a, b) => checkType(x, a, ctx).map(_ => b)
      case _                     => None
    }
    case Expr.Lam(_, _) => None
  }

  def checkType(e: Expr, t: Type, ctx: Map[String, Type]): Option[Unit] = e match {
    case Expr.Lit(_) => Option.when(t == Type.Int)(())
    case Expr.Var(name) => ctx.get(name).fold(Some(()))(t1 => Option.when(t == t1)(()))
    case Expr.App(f, x) => inferType(f, ctx).flatMap {
      case Type.Func(a, b) => for {
        _ <- checkType(x, a, ctx)
        _ <- Option.when(b == t)(())
      } yield ()
      case _ => None
    }
    case Expr.Lam(Expr.Var(x), b) => t match {
      case Type.Func(tx, tb) => checkType(b, tb, ctx + (x -> tx))
      case _ => None
    }
  }

  case class Func(name: String, tp: Type, args: List[Expr.Var], body: Expr)
