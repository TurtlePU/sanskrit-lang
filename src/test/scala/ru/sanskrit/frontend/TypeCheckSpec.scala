package ru.sanskrit.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.Expr

class TypeCheckSpec extends AnyFlatSpec with Matchers:
  "checkType" should "check literals to int" in {
    typecheck.checkType(Expr.Lit(42), Type.Int, Map.empty) shouldBe Some(())
  }

  it should "check variable in context" in {
    typecheck.checkType(Expr.Var("x", None), Type.Int, Map("x" -> Type.Int)) shouldBe Some(())
  }

  it should "check variable not in context" in {
    typecheck.checkType(Expr.Var("x", None), Type.Int, Map.empty) shouldBe Some(())
  }

  "inferType" should "infer variable type from context" in {
    typecheck.inferType(Expr.Var("x", None), Map("x" -> Type.Int)) shouldBe Some(Type.Int)
  }

  it should "fail on variable not in context" in {
    typecheck.inferType(Expr.Var("x", None), Map.empty) shouldBe None
  }