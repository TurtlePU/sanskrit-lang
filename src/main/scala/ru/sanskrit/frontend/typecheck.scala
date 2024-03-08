package ru.sanskrit.frontend

import cats.Id
import cats.parse.Caret
import cats.syntax.traverse.*
import ru.sanskrit.frontend.syntax.{Expr, Func}
import ru.sanskrit.common.Type

object typecheck:
  case class TypeCheckError(cause: String, caret: Caret)

  def inferType(e: Expr[Option], ctx: Map[String, Type]): Either[TypeCheckError, Expr[Id]] = e match {
    case Expr.Lit(l, c)       => Right(Expr.Lit(l, c))
    case Expr.Var(name, t, c) =>
      ctx.get(name).fold(t.toRight(TypeCheckError(s"Cannot find the type of variable $name", c)))(tp =>
        t.fold(Right(tp))(t1 =>
          Either.cond(tp == t1, tp, TypeCheckError(s"Variable $name should have types $tp and $t1 at the same type", c))
        )
      ).map(t => Expr.Var(name, t, c))
    case Expr.App(f, x, _, c) => inferType(f, ctx).flatMap { f1 =>
      f1.getType match {
        case Type.Func(a, b) => updateType(x, a).flatMap(x1 => inferType(x1, ctx)).map(a1 => Expr.App(f1, a1, b, c))
        case _               => Left(TypeCheckError("Application can be used only with functions", c))
      }
    }
    case Expr.Lam(Expr.Var(x, _, _), b, t, c) => t.toRight(TypeCheckError("Function must have a type", c)).flatMap {
      case Type.Func(tx, tb) => updateType(b, tb).flatMap(b1 => inferType(b1, ctx + (x -> tx)))
      case _                 => Left(TypeCheckError("Lambda's type is not a function", c))
    }
    case Expr.InfixOp(Expr.Var(f, _, c1), x, y, t, c2) => f match {
      case "+" | "*" =>
        for {
          x1 <- updateType(x, Type.Int).flatMap(inferType(_, ctx))
          y1 <- updateType(y, Type.Int).flatMap(inferType(_, ctx))
        } yield Expr.InfixOp(Expr.Var(f, Type.Func(Type.Int, Type.Func(Type.Int, Type.Int)), c1), x1, y1, Type.Int, c2)
      case _ => Left(TypeCheckError("Infix op can be only + and *", c2))
    }
  }

  private def updateType(e: Expr[Option], `type`: Type): Either[TypeCheckError, Expr[Option]] = e match {
    case Expr.Lit(l, c) if `type` == Type.Int                 => Right(Expr.Lit(l, c))
    case Expr.Var(name, t, c) if t.forall(_ == `type`)        => Right(Expr.Var(name, Some(`type`), c))
    case Expr.App(f, x, t, c) if t.forall(_ == `type`)        => Right(Expr.App(f, x, Some(`type`), c))
    case Expr.Lam(x, b, t, c) if t.forall(_ == `type`)        => Right(Expr.Lam(x, b, Some(`type`), c))
    case Expr.InfixOp(f, x, y, t, c) if t.forall(_ == `type`) => Right(Expr.InfixOp(f, x, y, Some(`type`), c))
    case _                                                    =>
      Left(TypeCheckError(s"Cannot update ($e)'s type to ${`type`}", e.getCaret))
  }

  def inferFuncType(f: Func[Option]): Either[TypeCheckError, Func[Id]] = {
    val ctx = f.args.flatMap (v => v.`type`.map (t => v.name -> t) ).toMap
    for {
      updatedBody <- f.tp.fold(Right(f.body))(t => updateType(f.body, t))
      typedBody   <- inferType(updatedBody, ctx)
      typedArgs   <- f.args.traverse(arg => getType(typedBody, arg.name).flatMap(t => updateVarToId(arg, t)))
    } yield Func(f.name, typedBody.getType, typedBody, typedArgs: _*)
  }

  private def updateVarToId(e: Expr.Var[Option], `type`: Type): Either[TypeCheckError, Expr.Var[Id]] = e match {
    case Expr.Var(name, t, c) if t.forall(_ == `type`) => Right(Expr.Var(name, `type`, c))
    case _                                             =>
      Left(TypeCheckError("Cannot update not a var to a typed expression", e.getCaret))
  }

  private def getType(e: Expr[Id], name: String): Either[TypeCheckError, Type] = e match {
    case Expr.Var(n, t, _) if n == name         => Right(t)
    case Expr.App(f, x, _, _)                   => getType(f, name).orElse(getType(x, name))
    case Expr.Lam(x, b, _, _) if x.name != name => getType(b, name)
    case _                                      =>
      Left(TypeCheckError(s"Cannon find type of the variable $name", e.getCaret))
  }
