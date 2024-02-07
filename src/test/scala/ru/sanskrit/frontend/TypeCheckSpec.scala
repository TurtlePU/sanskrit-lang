package ru.sanskrit.frontend

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.Expr

class TypeCheckSpec extends AnyFlatSpec with Matchers:
  "checkType" should "check literals to int" in {
    typecheck.inferType(Expr.Lit(42), Map.empty) shouldBe Some(Expr.Lit(42))
  }

  it should "check variable in context" in {
    typecheck.inferType(Expr.Var("x", Some(Type.Int)), Map("x" -> Type.Int)) shouldBe Some(Expr.Var[Id]("x", Type.Int))
  }

  it should "check variable not in context" in {
    typecheck.inferType(Expr.Var("x", Some(Type.Int)), Map.empty) shouldBe Some(Expr.Var[Id]("x", Type.Int))
  }

  "inferType" should "infer variable type from context" in {
    typecheck.inferType(Expr.Var("x", None), Map("x" -> Type.Int)) shouldBe Some(Expr.Var[Id]("x", Type.Int))
  }

  it should "fail on variable not in context" in {
    typecheck.inferType(Expr.Var("x", None), Map.empty) shouldBe None
  }