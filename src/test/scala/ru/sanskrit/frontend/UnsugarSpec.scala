package ru.sanskrit.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.{Expr, Name, Rhs, Type}
import ru.sanskrit.frontend.syntax.{Expr => FExpr}
import ru.sanskrit.frontend.typecheck.Func

class UnsugarSpec extends AnyFlatSpec with Matchers:
  "unsugarExpr" should "unsugar literal" in {
    unsugar.unsugarExpr(FExpr.Lit(42)) shouldBe Rhs.Val(Expr.Val.Lit(42))
  }

  it should "unsugar variable name" in {
    unsugar.unsugarExpr(FExpr.Var("test")) shouldBe Rhs.Val(Expr.Val.Var(Name("test")))
  }

  it should "unsugar application" in {
    unsugar.unsugarExpr(FExpr.App(FExpr.Var("f"), FExpr.Var("x"))) shouldBe
      Rhs.App(Rhs.Val(Expr.Val.Var(Name("f"))), Rhs.Val(Expr.Val.Var(Name("x"))))
  }

  it should "unsugar lambda" in {
    unsugar.unsugarExpr(FExpr.Lam(FExpr.Var("x"), FExpr.Lit(42))) shouldBe
      Rhs.Abs(Name("x"), Rhs.Val(Expr.Val.Lit(42)))
  }

  "unsugarProgram" should "unsugar simple program" in {
    unsugar.unsugarProgram(List(Func("main", Type.Int, List.empty, FExpr.Lit(42)))) shouldBe
      Some(Expr.Let(Name("main"), Type.Int, Rhs.Val(Expr.Val.Lit(42)), Expr.Val.Var(Name("main"))))
  }

  it should "fail on program without main" in {
    unsugar.unsugarProgram(List.empty) shouldBe None
  }
