package ru.sanskrit.frontend

import cats.Id
import ru.sanskrit.frontend.syntax.Expr
import ru.sanskrit.common.Type

object typecheck:
  def inferType(e: Expr[Option], ctx: Map[String, Type]): Option[Expr[Id]] = e match {
    case Expr.Lit(l)       => Some(Expr.Lit(l))
    case Expr.Var(name, _) => ctx.get(name).map(t => Expr.Var(name, t))
    case Expr.App(f, x, _) => inferType(f, ctx).flatMap { f1 =>
      f1.getType match {
        case Type.Func(a, b) => checkType(x, a, ctx).map(a1 => Expr.App(f1, a1, b))
        case _ => None
      }
    }
    case Expr.Lam(_, _, _) => None
  }

  def checkType(e: Expr[Option], t: Type, ctx: Map[String, Type]): Option[Expr[Id]] = e match {
    case Expr.Lit(l) => Option.when(t == Type.Int)(Expr.Lit(l))
    case Expr.Var(name, _) =>
      ctx.get(name).fold[Option[Expr[Id]]](Some(Expr.Var(name, t)))(t1 => Option.when(t == t1)(Expr.Var(name, t)))
    case Expr.App(f, x, _) => inferType(f, ctx).flatMap { f1 =>
      f1.getType match {
        case Type.Func(a, b) => for {
            x1 <- checkType(x, a, ctx)
            _ <- Option.when(b == t)(())
          } yield Expr.App(f1, x1, b)
        case _ => None
      }
    }
    case Expr.Lam(Expr.Var(x, _), b, _) => t match {
      case Type.Func(tx, tb) => checkType(b, tb, ctx + (x -> tx))
      case _ => None
    }
  }

  case class Func(name: String, tp: Type, args: List[Expr.Var[Id]], body: Expr[Id])
