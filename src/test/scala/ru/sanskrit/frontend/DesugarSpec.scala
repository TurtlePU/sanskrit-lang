package ru.sanskrit.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.{App, Abs, Expr, Let, Lit, Name, Type, Var}
import ru.sanskrit.frontend.syntax.{Expr => FExpr, Func}

class DesugarSpec extends AnyFlatSpec with Matchers:
  "desugarExpr" should "desugar literal" in {
    desugar.desugarExpr(FExpr.Lit(42)) shouldBe Some(Lit(42), List.empty)
  }

  it should "desugar variable name" in {
    desugar.desugarExpr(FExpr.Var("test", Type.Int)) shouldBe Some(Var(Name("test")), List.empty)
  }

  it should "desugar application" in {
    desugar.desugarExpr(FExpr.App(FExpr.Var("f", Type.Func(Type.Int, Type.Int)), FExpr.Var("x", Type.Int), Type.Int)) shouldBe
      Some(App(Var(Name("f")), Var(Name("x"))), List.empty)
  }

  it should "desugar lambda" in {
    desugar.desugarExpr(FExpr.Lam(FExpr.Var("x", Type.Int), FExpr.Lit(42), Type.Func(Type.Int, Type.Int))) shouldBe
      Some(Abs(Name("x"), Lit(42)), List())
  }

  "desugarProgram" should "desugar simple program" in {
    desugar.desugarProgram(List(Func("main", Type.Int, FExpr.Lit(42)))) shouldBe
      Some(Let(Name("main"), Type.Int, Lit(42), Var(Name("main"))))
  }

  it should "fail on program without main" in {
    desugar.desugarProgram(List.empty) shouldBe None
  }
