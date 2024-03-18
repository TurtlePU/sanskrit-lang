package ru.sanskrit.frontend

import cats.Id
import ru.sanskrit.common.{Abs, App, Expr, Let, Lit, Mul, Name, Sum, Type, Var}
import ru.sanskrit.frontend.syntax.{Expr => FExpr, Func}

import java.util.UUID

object desugar:
  def desugarProgram(f: List[Func[Id]]): Option[Expr] =
    for {
      _   <- Option.when(f.exists(_.name == "main"))(())
      res <-
        f.foldRight[Option[Expr]](Some(Var(Name("main"))))((f, accF) =>
          for {
            acc         <- accF
            (rhs, lets) <- desugarExpr(f.body)
          } yield lets.foldRight(Let(Name(f.name), f.tp, rhs, acc)) { case ((n, t, r), acc) =>
            Let(n, t, r, acc)
          }
        )
    } yield res

  def desugarExpr(e: FExpr[Id]): Option[(Expr, List[(Name, Type, Expr)])] = e match {
    case FExpr.Lit(x, _)       => Some((Lit(x), List.empty))
    case FExpr.Var(name, _, _) => Some((Var(Name(name)), List.empty))
    case FExpr.App(f, a, _, _) =>
      for {
        (fVal, fLets) <- desugarExpr(f)
        (aVal, aLets) <- desugarExpr(a)
      } yield {
        val fName = s"f$$${UUID.randomUUID()}"
        val aName = s"a$$${UUID.randomUUID()}"
        (
          App(Var(Name(fName)), Var(Name(aName))),
          fLets ++ aLets :+ (Name(fName), f.getType, fVal) :+ (Name(aName), a.getType, aVal)
        )
      }
    case FExpr.Lam(FExpr.Var(x, _, _), a, _, _) =>
      for {
        (aVal, aLets) <- desugarExpr(a)
      } yield {
        val fName = s"f$$${UUID.randomUUID()}"
        (Abs(Name(x), Let(Name(fName), a.getType, aVal, Var(Name(fName)))), aLets)
      }
    case FExpr.InfixOp(FExpr.Var(f, _, _), x, y, _, _) =>
      for {
        (xVal, xLets) <- desugarExpr(x)
        (yVal, yLets) <- desugarExpr(y)
        xName          = s"x$$${UUID.randomUUID()}"
        yName          = s"y$$${UUID.randomUUID()}"
        xVar           = Var(Name(xName))
        yVar           = Var(Name(yName))
        lets           = xLets ++ yLets :+ (Name(xName), x.getType, xVal) :+ (Name(yName), y.getType, yVal)
        res           <- f match {
          case "+" => Some(Sum(xVar, yVar))
          case "*" => Some(Mul(xVar, yVar))
          case _   => None
        }
      } yield (res, lets)
  }
