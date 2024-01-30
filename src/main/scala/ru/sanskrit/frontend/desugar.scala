package ru.sanskrit.frontend

import ru.sanskrit.common.{Expr, Name, Rhs, Type}
import ru.sanskrit.frontend.syntax.{Expr => FExpr}
import ru.sanskrit.frontend.typecheck.Func

import java.util.UUID

object desugar:
  def desugarProgram(f: List[Func]): Option[Expr] =
    for {
      _   <- Option.when(f.exists(_.name == "main"))(())
      res <-
        f.foldRight[Option[Expr]](Some(Expr.Val.Var(Name("main"))))((f, accF) =>
          for {
            acc         <- accF
            (rhs, lets) <- desugarExpr(f.body)
          } yield lets.foldRight(Expr.Let(Name(f.name), f.tp, rhs, acc)) { case ((n, t, r), acc) =>
            Expr.Let(n, t, r, acc)
          }
        )
    } yield res

  def desugarExpr(e: FExpr[Option]): Option[(Rhs, List[(Name, Type, Rhs)])] = e match {
    case FExpr.Lit(x)       => Some((Rhs.Val(Expr.Val.Lit(x)), List.empty))
    case FExpr.Var(name, _) => Some((Rhs.Val(Expr.Val.Var(Name(name))), List.empty))
    case FExpr.App(f, a, _) =>
      for {
        (fVal, fLets) <- desugarExpr(f)
        (aVal, aLets) <- desugarExpr(a)
        fType         <- typecheck.inferType(f, Map.empty)
        aType         <- typecheck.inferType(a, Map.empty)
      } yield {
        val fName = s"f$$${UUID.randomUUID()}"
        val aName = s"a$$${UUID.randomUUID()}"
        (
          Rhs.App(Expr.Val.Var(Name(fName)), Expr.Val.Var(Name(aName))),
          fLets ++ aLets :+ (Name(fName), fType, fVal) :+ (Name(aName), aType, aVal)
        )
      }
    case FExpr.Lam(FExpr.Var(x, _), a, _) =>
      for {
        (aVal, aLets) <- desugarExpr(a)
        aType         <- typecheck.inferType(a, Map.empty)
      } yield {
        val fName = s"f$$${UUID.randomUUID()}"
        (Rhs.Abs(Name(x), Expr.Let(Name(fName), aType, aVal, Expr.Val.Var(Name(fName)))), aLets)
      }
  }
