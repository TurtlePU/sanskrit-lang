package ru.sanskrit.frontend

import cats.Id
import cats.parse.Caret
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.Type
import ru.sanskrit.frontend.syntax.{Expr, Func}
import ru.sanskrit.frontend.typecheck.TypeCheckError

class TypeCheckSpec extends AnyFlatSpec with Matchers:
  private val testCaret = Caret(0, 0, 0)

  "checkType" should "check literals to int" in {
    typecheck.inferType(Expr.Lit(42, testCaret), Map.empty) shouldBe Right(Expr.Lit(42, testCaret))
  }

  it should "check a variable in the context" in {
    typecheck.inferType(Expr.Var("x", Some(Type.Int), testCaret), Map("x" -> Type.Int)) shouldBe
      Right(Expr.Var[Id]("x", Type.Int, testCaret))
  }

  it should "check a variable not in the context" in {
    typecheck.inferType(Expr.Var("x", Some(Type.Int), testCaret), Map.empty) shouldBe
      Right(Expr.Var[Id]("x", Type.Int, testCaret))
  }

  "inferType" should "infer a variable type from the context" in {
    typecheck.inferType(Expr.Var("x", None, testCaret), Map("x" -> Type.Int)) shouldBe
      Right(Expr.Var[Id]("x", Type.Int, testCaret))
  }

  it should "fail on a variable not in the context" in {
    typecheck.inferType(Expr.Var("x", None, testCaret), Map.empty) shouldBe
      Left(TypeCheckError("Cannot find the type of variable x", testCaret))
  }

  it should "infer int expr type" in {
    typecheck.inferType(
      Expr.InfixOp(
        Expr.Var("+", None, testCaret),
        Expr.Lit(42, testCaret),
        Expr.Var("x", None, testCaret),
        None,
        testCaret
      ),
      Map.empty
    ) shouldBe
      Right(Expr.InfixOp[Id](
        Expr.Var("+", Type.Func(Type.Int, Type.Func(Type.Int, Type.Int)), testCaret),
        Expr.Lit(42, testCaret),
        Expr.Var("x", Type.Int, testCaret),
        Type.Int,
        testCaret
      ))
  }

  it should "fail on non int arguments" in {
    typecheck.inferType(
      Expr.InfixOp(
        Expr.Var("+", None, testCaret),
        Expr.Lit(42, testCaret),
        Expr.Var("x", Some(Type.Func(Type.Int, Type.Int)), testCaret),
        None,
        testCaret
      ),
      Map.empty
    ) shouldBe Left(TypeCheckError(s"Cannot update (${Expr.Var("x", Some(Type.Func(Type.Int, Type.Int)), testCaret)})'s type to Int", testCaret))
  }

  "inferFuncType" should "accept a fully annotated function" in {
    typecheck.inferFuncType(
      Func[Option]("id", Some(Type.Int), Expr.Var("x", None, testCaret), Expr.Var("x", Some(Type.Int), testCaret))
    ) shouldBe
      Right(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int, testCaret), Expr.Var("x", Type.Int, testCaret)))
  }

  it should "infer a function type based on the argument types" in {
    typecheck.inferFuncType(
      Func[Option]("id", None, Expr.Var("x", None, testCaret), Expr.Var("x", Some(Type.Int), testCaret))
    ) shouldBe
      Right(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int, testCaret), Expr.Var("x", Type.Int, testCaret)))
  }

  it should "infer a argument type based on the function types" in {
    typecheck.inferFuncType(
      Func[Option]("id", Some(Type.Int), Expr.Var("x", None, testCaret), Expr.Var("x", None, testCaret))
    ) shouldBe
      Right(Func[Id]("id", Type.Int, Expr.Var("x", Type.Int, testCaret), Expr.Var("x", Type.Int, testCaret)))
  }
