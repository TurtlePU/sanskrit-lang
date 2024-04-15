package ru.sanskrit.frontend

import cats.Id
import cats.syntax.traverse.*
import ru.sanskrit.frontend.syntax.{Expr, Func, Position}
import ru.sanskrit.common.Type

object typecheck:
  case class TypeCheckError(cause: String, position: Position)

  def inferType(e: Expr[Option], ctx: Map[String, Type]): Either[TypeCheckError, Expr[Id]] = e match {
    case Expr.Lit(l, p)       => Right(Expr.Lit(l, p))
    case Expr.Var(name, t, p) =>
      ctx.get(name).fold(t.toRight(TypeCheckError(s"Cannot find the type of variable $name", p)))(tp =>
        t.fold(Right(tp))(t1 =>
          Either.cond(tp == t1, tp, TypeCheckError(s"Variable $name should have types $tp and $t1 at the same type", p))
        )
      ).map(t => Expr.Var(name, t, p))
    case Expr.App(f, x, _, p) => inferType(f, ctx).flatMap { f1 =>
      f1.getType match {
        case Type.Func(a, b) => updateType(x, a).flatMap(x1 => inferType(x1, ctx)).map(a1 => Expr.App(f1, a1, b, p))
        case _               => Left(TypeCheckError("Application can be used only with functions", p))
      }
    }
    case Expr.Lam(Expr.Var(x, _, _), b, t, p) => t.toRight(TypeCheckError("Function must have a type", p)).flatMap {
      case Type.Func(tx, tb) => updateType(b, tb).flatMap(b1 => inferType(b1, ctx + (x -> tx)))
      case _                 => Left(TypeCheckError("Lambda's type is not a function", p))
    }
    case Expr.InfixOp(Expr.Var(f, _, p1), x, y, t, p2) => f match {
      case "+" | "*" =>
        for {
          x1 <- updateType(x, Type.Int).flatMap(inferType(_, ctx))
          y1 <- updateType(y, Type.Int).flatMap(inferType(_, ctx))
        } yield Expr.InfixOp(Expr.Var(f, Type.Func(Type.Int, Type.Func(Type.Int, Type.Int)), p1), x1, y1, Type.Int, p2)
      case _ => Left(TypeCheckError("Infix op can be only + and *", p2))
    }
  }

  private def updateType(e: Expr[Option], `type`: Type): Either[TypeCheckError, Expr[Option]] = e match {
    case Expr.Lit(l, p) if `type` == Type.Int                 => Right(Expr.Lit(l, p))
    case Expr.Var(name, t, p) if t.forall(_ == `type`)        => Right(Expr.Var(name, Some(`type`), p))
    case Expr.App(f, x, t, p) if t.forall(_ == `type`)        => Right(Expr.App(f, x, Some(`type`), p))
    case Expr.Lam(x, b, t, p) if t.forall(_ == `type`)        => Right(Expr.Lam(x, b, Some(`type`), p))
    case Expr.InfixOp(f, x, y, t, p) if t.forall(_ == `type`) => Right(Expr.InfixOp(f, x, y, Some(`type`), p))
    case _                                                    =>
      Left(TypeCheckError(s"Cannot update ($e)'s type to ${`type`}", e.getPosition))
  }

  def inferFuncType(f: Func[Option]): Either[TypeCheckError, Func[Id]] = {
    val ctx = f.args.flatMap(v => v.`type`.map(t => v.name -> t)).toMap
    for {
      updatedBody <- f.tp.fold(Right(f.body))(t => updateType(f.body, t))
      typedBody   <- inferType(updatedBody, ctx)
      typedArgs   <- f.args.traverse(arg => getType(typedBody, arg.name).flatMap(t => updateVarToId(arg, t)))
    } yield Func(f.name, typedBody.getType, typedBody, typedArgs*)
  }

  private def updateVarToId(e: Expr.Var[Option], `type`: Type): Either[TypeCheckError, Expr.Var[Id]] = e match {
    case Expr.Var(name, t, p) if t.forall(_ == `type`) => Right(Expr.Var(name, `type`, p))
    case _                                             =>
      Left(TypeCheckError("Cannot update not a var to a typed expression", e.getPosition))
  }

  private def getType(e: Expr[Id], name: String): Either[TypeCheckError, Type] = e match {
    case Expr.Var(n, t, _) if n == name         => Right(t)
    case Expr.App(f, x, _, _)                   => getType(f, name).orElse(getType(x, name))
    case Expr.Lam(x, b, _, _) if x.name != name => getType(b, name)
    case _                                      =>
      Left(TypeCheckError(s"Cannon find type of the variable $name", e.getPosition))
  }
