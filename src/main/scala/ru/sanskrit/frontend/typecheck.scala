package ru.sanskrit.frontend

import cats.Id
import ru.sanskrit.frontend.syntax.Expr
import ru.sanskrit.common.Type

object typecheck:
  def inferType(e: Expr[Option], ctx: Map[String, Type]): Option[Expr[Id]] = e match {
    case Expr.Lit(l)            => Some(Expr.Lit(l))
    case Expr.Var(name, t) =>
      ctx.get(name).fold(t)(tp => t.fold(Some(tp))(t1 => Option.when(tp == t1)(tp))).map(t => Expr.Var(name, t))
    case Expr.App(f, x, _) => inferType(f, ctx).flatMap { f1 =>
      f1.getType match {
        case Type.Func(a, b) => updateType(x, a).flatMap(x1 => inferType(x1, ctx)).map(a1 => Expr.App(f1, a1, b))
        case _ => None
      }
    }
    case Expr.Lam(Expr.Var(x, _), b, t) => t.flatMap {
      case Type.Func(tx, tb) => updateType(b, tb).flatMap(b1 => inferType(b1, ctx + (x -> tx)))
      case _                 => None
    }
  }

  def updateType(e: Expr[Option], `type`: Type): Option[Expr[Option]] = e match {
    case Expr.Lit(l) if `type` == Type.Int          => Some(Expr.Lit(l))
    case Expr.Var(name, t) if t.forall(_ == `type`) => Some(Expr.Var(name, Some(`type`)))
    case Expr.App(f, x, t) if t.forall(_ == `type`) => Some(Expr.App(f, x, Some(`type`)))
    case Expr.Lam(x, b, t) if t.forall(_ == `type`) => Some(Expr.Lam(x, b, Some(`type`)))
    case _ => None
  }

  case class Func(name: String, tp: Type, args: List[Expr.Var[Id]], body: Expr[Id])
