package ru.sanskrit.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.{Expr, Name, Rhs, Type}
import ru.sanskrit.frontend.syntax.{Expr => FExpr}
import ru.sanskrit.frontend.typecheck.Func

class DesugarSpec extends AnyFlatSpec with Matchers:
  "desugarExpr" should "desugar literal" in {
    desugar.desugarExpr(FExpr.Lit(42)) shouldBe Rhs.Val(Expr.Val.Lit(42))
  }

  it should "desugar variable name" in {
    desugar.desugarExpr(FExpr.Var("test")) shouldBe Rhs.Val(Expr.Val.Var(Name("test")))
  }

  it should "desugar application" in {
    desugar.desugarExpr(FExpr.App(FExpr.Var("f"), FExpr.Var("x"))) shouldBe
      Rhs.App(Rhs.Val(Expr.Val.Var(Name("f"))), Rhs.Val(Expr.Val.Var(Name("x"))))
  }

  it should "desugar lambda" in {
    desugar.desugarExpr(FExpr.Lam(FExpr.Var("x"), FExpr.Lit(42))) shouldBe
      Rhs.Abs(Name("x"), Rhs.Val(Expr.Val.Lit(42)))
  }

  "desugarProgram" should "desugar simple program" in {
    desugar.desugarProgram(List(Func("main", Type.Int, List.empty, FExpr.Lit(42)))) shouldBe
      Some(Expr.Let(Name("main"), Type.Int, Rhs.Val(Expr.Val.Lit(42)), Expr.Val.Var(Name("main"))))
  }

  it should "fail on program without main" in {
    desugar.desugarProgram(List.empty) shouldBe None
  }
