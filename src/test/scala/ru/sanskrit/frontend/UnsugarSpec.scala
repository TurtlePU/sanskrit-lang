package ru.sanskrit.frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.sanskrit.common.{Expr, Rhs}
import ru.sanskrit.frontend.syntax.{Expr => FExpr}

class UnsugarSpec extends AnyFlatSpec with Matchers:
  "unsugar" should "unsugar literal" in {
    unsugar.unsugarExpr(FExpr.Lit(42)) shouldBe Rhs.Val(Expr.Val.Lit(42))
  }
