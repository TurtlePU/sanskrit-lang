package ru.sanskrit.frontend

import cats.Id
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func}

class TypeCheckSpec extends AnyFlatSpec with Matchers:
  "checkType" should "check literals to int" in {
    typecheck.inferType(Expr.Lit(42), Map.empty) shouldBe Some(Expr.Lit(42))
  }

  it should "check a variable in the context" in {
    typecheck.inferType(Expr.Var("x", Some(Type.Int)), Map("x" -> Type.Int)) shouldBe Some(Expr.Var[Id]("x", Type.Int))
  }

  it should "check a variable not in the context" in {
    typecheck.inferType(Expr.Var("x", Some(Type.Int)), Map.empty) shouldBe Some(Expr.Var[Id]("x", Type.Int))
  }

  "inferType" should "infer a variable type from the context" in {
    typecheck.inferType(Expr.Var("x", None), Map("x" -> Type.Int)) shouldBe Some(Expr.Var[Id]("x", Type.Int))
  }

  it should "fail on a variable not in the context" in {
    typecheck.inferType(Expr.Var("x", None), Map.empty) shouldBe None
  }

  it should "infer int expr type" in {
    typecheck.inferType(Expr.InfixOp(Expr.Var("+", None), Expr.Lit(42), Expr.Var("x", None), None), Map.empty) shouldBe
      Some(Expr.InfixOp[Id](
        Expr.Var("+", Type.Func(Type.Int, Type.Func(Type.Int, Type.Int))),
        Expr.Lit(42),
        Expr.Var("x", Type.Int),
        Type.Int
      ))
  }

  it should "fail on non int arguments" in {
    typecheck.inferType(
      Expr.InfixOp(Expr.Var("+", None), Expr.Lit(42), Expr.Var("x", Some(Type.Func(Type.Int, Type.Int))), None),
      Map.empty
    ) shouldBe None
  }

  "inferFuncType" should "accept a fully annotated function" in {
    typecheck.inferFuncType(Func[Option]("id", Some(Type.Int), Expr.Var("x", None), Expr.Var("x", Some(Type.Int)))) shouldBe
      Some(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int), Expr.Var("x", Type.Int)))
  }

  it should "infer a function type based on the argument types" in {
    typecheck.inferFuncType(Func[Option]("id", None, Expr.Var("x", None), Expr.Var("x", Some(Type.Int)))) shouldBe
      Some(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int), Expr.Var("x", Type.Int)))
  }

  it should "infer a argument type based on the function types" in {
    typecheck.inferFuncType(Func[Option]("id", Some(Type.Int), Expr.Var("x", None), Expr.Var("x", None))) shouldBe
      Some(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int), Expr.Var("x", Type.Int)))
  }
