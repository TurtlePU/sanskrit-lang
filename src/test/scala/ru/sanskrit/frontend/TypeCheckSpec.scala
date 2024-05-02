package ru.sanskrit.frontend

import cats.Id
import cats.parse.Caret
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func, Position}
import ru.sanskrit.frontend.typecheck.TypeCheckError

class TypeCheckSpec extends AnyFlatSpec with Matchers:
  private val testCaret = Caret(0, 0, 0)
  private val testPosition = Position(testCaret, testCaret)

  "checkType" should "check literals to int" in {
    typecheck.inferType(Expr.Lit(42, testPosition), Map.empty) shouldBe Right(Expr.Lit(42, testPosition))
  }

  it should "check a variable in the context" in {
    typecheck.inferType(Expr.Var("x", Some(Type.Int), testPosition), Map("x" -> Type.Int)) shouldBe
      Right(Expr.Var[Id]("x", Type.Int, testPosition))
  }

  it should "check a variable not in the context" in {
    typecheck.inferType(Expr.Var("x", Some(Type.Int), testPosition), Map.empty) shouldBe
      Right(Expr.Var[Id]("x", Type.Int, testPosition))
  }

  "inferType" should "infer a variable type from the context" in {
    typecheck.inferType(Expr.Var("x", None, testPosition), Map("x" -> Type.Int)) shouldBe
      Right(Expr.Var[Id]("x", Type.Int, testPosition))
  }

  it should "fail on a variable not in the context" in {
    typecheck.inferType(Expr.Var("x", None, testPosition), Map.empty) shouldBe
      Left(TypeCheckError("Cannot find the type of variable x", testPosition))
  }

  it should "infer int expr type" in {
    typecheck.inferType(
      Expr.InfixOp(
        Expr.Var("+", None, testPosition),
        Expr.Lit(42, testPosition),
        Expr.Var("x", None, testPosition),
        None,
        testPosition
      ),
      Map.empty
    ) shouldBe
      Right(Expr.InfixOp[Id](
        Expr.Var("+", Type.Func(Type.Int, Type.Func(Type.Int, Type.Int)), testPosition),
        Expr.Lit(42, testPosition),
        Expr.Var("x", Type.Int, testPosition),
        Type.Int,
        testPosition
      ))
  }

  it should "fail on non int arguments" in {
    typecheck.inferType(
      Expr.InfixOp(
        Expr.Var("+", None, testPosition),
        Expr.Lit(42, testPosition),
        Expr.Var("x", Some(Type.Func(Type.Int, Type.Int)), testPosition),
        None,
        testPosition
      ),
      Map.empty
    ) shouldBe Left(
      TypeCheckError(
        s"Cannot update (${Expr.Var("x", Some(Type.Func(Type.Int, Type.Int)), testPosition)})'s type to Int",
        testPosition
      )
    )
  }

  "inferFuncType" should "accept a fully annotated function" in {
    typecheck.inferFuncType(
      Func[Option](
        "id",
        Some(Type.Int),
        Expr.Var("x", None, testPosition),
        Expr.Var("x", Some(Type.Int), testPosition)
      ),
      Map.empty
    ) shouldBe
      Right(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int, testPosition), Expr.Var("x", Type.Int, testPosition)))
  }

  it should "infer a function type based on the argument types" in {
    typecheck.inferFuncType(
      Func[Option]("id", None, Expr.Var("x", None, testPosition), Expr.Var("x", Some(Type.Int), testPosition)),
      Map.empty
    ) shouldBe
      Right(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int, testPosition), Expr.Var("x", Type.Int, testPosition)))
  }

  it should "infer a argument type based on the function types" in {
    typecheck.inferFuncType(
      Func[Option]("id", Some(Type.Int), Expr.Var("x", None, testPosition), Expr.Var("x", None, testPosition)),
      Map.empty
    ) shouldBe
      Right(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int, testPosition), Expr.Var("x", Type.Int, testPosition)))
  }
