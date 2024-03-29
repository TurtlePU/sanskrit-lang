package ru.sanskrit.frontend

import cats.Id
import cats.syntax.traverse.*
import ru.sanskrit.frontend.syntax.{Expr, Func}
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
    case Expr.InfixOp(Expr.Var(f, _), x, y, t) => f match {
      case "+" | "*" =>
        for {
          x1 <- updateType(x, Type.Int).flatMap(inferType(_, ctx))
          y1 <- updateType(y, Type.Int).flatMap(inferType(_, ctx))
        } yield Expr.InfixOp(Expr.Var(f, Type.Func(Type.Int, Type.Func(Type.Int, Type.Int))), x1, y1, Type.Int)
      case _ => None
    }
  }

  private def updateType(e: Expr[Option], `type`: Type): Option[Expr[Option]] = e match {
    case Expr.Lit(l) if `type` == Type.Int                 => Some(Expr.Lit(l))
    case Expr.Var(name, t) if t.forall(_ == `type`)        => Some(Expr.Var(name, Some(`type`)))
    case Expr.App(f, x, t) if t.forall(_ == `type`)        => Some(Expr.App(f, x, Some(`type`)))
    case Expr.Lam(x, b, t) if t.forall(_ == `type`)        => Some(Expr.Lam(x, b, Some(`type`)))
    case Expr.InfixOp(f, x, y, t) if t.forall(_ == `type`) => Some(Expr.InfixOp(f, x, y, Some(`type`)))
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
    case Expr.Var(name, t) if t.forall(_ == `type`) => Some(Expr.Var(name, `type`))
    case _ => None
  }

  private def getType(e: Expr[Id], name: String): Option[Type] = e match {
    case Expr.Var(n, t) if n == name         => Some(t)
    case Expr.App(f, x, _)                   => getType(f, name).orElse(getType(x, name))
    case Expr.Lam(x, b, _) if x.name != name => getType(b, name)
    case _                                   => None
  }
