package ru.sanskrit.frontend

import ru.sanskrit.common.{Expr, Name, Rhs}
import ru.sanskrit.frontend.syntax.{Expr => FExpr}
import ru.sanskrit.frontend.typecheck.Func

object desugar:
  def desugarProgram(f: List[Func]): Option[Expr] =
    Option.when(f.exists(_.name == "main"))(
      f.foldRight[Expr](Expr.Val.Var(Name("main")))((f, acc) => Expr.Let(Name(f.name), f.tp, desugarExpr(f.body), acc))
    )

  def desugarExpr(e: FExpr): Rhs = e match {
    case FExpr.Lit(x)               => Rhs.Val(Expr.Val.Lit(x))
    case FExpr.Var(name)            => Rhs.Val(Expr.Val.Var(Name(name)))
    case FExpr.App(f, a)            => Rhs.App(desugarExpr(f), desugarExpr(a))
    case FExpr.Lam(FExpr.Var(x), a) => Rhs.Abs(Name(x), desugarExpr(a))
  }
