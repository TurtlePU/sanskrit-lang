package ru.sanskrit.frontend

import cats.Id
import cats.parse.Caret
import cats.syntax.traverse.*
import ru.sanskrit.frontend.syntax.{Expr, Func}
import ru.sanskrit.common.Type

object typecheck:
  case class TypeCheckError(cause: String, caret: Caret)

  def inferType(e: Expr[Option], ctx: Map[String, Type]): Option[Expr[Id]] = e match {
    case Expr.Lit(l, c)       => Some(Expr.Lit(l, c))
    case Expr.Var(name, t, c) =>
      ctx.get(name).fold(t)(tp => t.fold(Some(tp))(t1 => Option.when(tp == t1)(tp))).map(t => Expr.Var(name, t, c))
    case Expr.App(f, x, _, c) => inferType(f, ctx).flatMap { f1 =>
      f1.getType match {
        case Type.Func(a, b) => updateType(x, a).flatMap(x1 => inferType(x1, ctx)).map(a1 => Expr.App(f1, a1, b, c))
        case _ => None
      }
    }
    case Expr.Lam(Expr.Var(x, _, _), b, t, _) => t.flatMap {
      case Type.Func(tx, tb) => updateType(b, tb).flatMap(b1 => inferType(b1, ctx + (x -> tx)))
      case _                 => None
    }
    case Expr.InfixOp(Expr.Var(f, _, c1), x, y, t, c2) => f match {
      case "+" | "*" =>
        for {
          x1 <- updateType(x, Type.Int).flatMap(inferType(_, ctx))
          y1 <- updateType(y, Type.Int).flatMap(inferType(_, ctx))
        } yield Expr.InfixOp(Expr.Var(f, Type.Func(Type.Int, Type.Func(Type.Int, Type.Int)), c1), x1, y1, Type.Int, c2)
      case _ => None
    }
  }

  private def updateType(e: Expr[Option], `type`: Type): Option[Expr[Option]] = e match {
    case Expr.Lit(l, c) if `type` == Type.Int                 => Some(Expr.Lit(l, c))
    case Expr.Var(name, t, c) if t.forall(_ == `type`)        => Some(Expr.Var(name, Some(`type`), c))
    case Expr.App(f, x, t, c) if t.forall(_ == `type`)        => Some(Expr.App(f, x, Some(`type`), c))
    case Expr.Lam(x, b, t, c) if t.forall(_ == `type`)        => Some(Expr.Lam(x, b, Some(`type`), c))
    case Expr.InfixOp(f, x, y, t, c) if t.forall(_ == `type`) => Some(Expr.InfixOp(f, x, y, Some(`type`), c))
    case _ => None
  }

  def inferFuncType(f: Func[Option]): Option[Func[Id]] = {
    val ctx = f.args.flatMap (v => v.`type`.map (t => v.name -> t) ).toMap
    for {
      updatedBody <- f.tp.fold(Some(f.body))(t => updateType(f.body, t))
      typedBody   <- inferType (updatedBody, ctx)
      typedArgs   <- f.args.traverse(arg => getType(typedBody, arg.name).flatMap(t => updateVarToId(arg, t)))
    } yield Func(f.name, typedBody.getType, typedBody, typedArgs: _*)
  }

  private def updateVarToId(e: Expr.Var[Option], `type`: Type): Option[Expr.Var[Id]] = e match {
    case Expr.Var(name, t, c) if t.forall(_ == `type`) => Some(Expr.Var(name, `type`, c))
    case _ => None
  }

  private def getType(e: Expr[Id], name: String): Option[Type] = e match {
    case Expr.Var(n, t, _) if n == name         => Some(t)
    case Expr.App(f, x, _, _)                   => getType(f, name).orElse(getType(x, name))
    case Expr.Lam(x, b, _, _) if x.name != name => getType(b, name)
    case _                                      => None
  }
