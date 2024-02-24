package ru.sanskrit.backend

import ru.sanskrit.common.{Expr, Name, Rhs, Type}
import scala.collection.mutable.Map

object interpreter:
  type Env = Map[Name, Rhs.Val]
  type FuncEnv = Map[Name, Rhs.Abs]

  def run(expr: Expr): Option[Rhs] = {
    Some(evaluate(expr, Map.empty, Map.empty))  // option is here for future error handling
  }

  private def evaluate(expr: Expr, env: Env, funcEnv: FuncEnv): Rhs.Val = expr match {
    case Expr.Let(x, t, v, b) =>
      t match {
        case Type.Int => env += (x -> evaluateRhs(v, env, funcEnv))
        case Type.Func(a, b) => funcEnv += (x -> v.asInstanceOf[Rhs.Abs])
      }
      evaluate(b, env, funcEnv)
    case v: Expr.Val => evaluateVal(v, env)
  }

  private def evaluateRhs(rhs: Rhs, env: Env, funcEnv: FuncEnv): Rhs.Val = rhs match {
    case r: Rhs.Val => r
    case Rhs.Sum(a, b) =>
      val valA = evaluateVal(a, env).v.asInstanceOf[Expr.Val.Lit].x
      val valB = evaluateVal(b, env).v.asInstanceOf[Expr.Val.Lit].x
      Rhs.Val(Expr.Val.Lit(valA + valB))
    case Rhs.Mul(a, b) =>
      val valA = evaluateVal(a, env).v.asInstanceOf[Expr.Val.Lit].x
      val valB = evaluateVal(b, env).v.asInstanceOf[Expr.Val.Lit].x
      Rhs.Val(Expr.Val.Lit(valA * valB))
    case Rhs.Abs(x, t) =>
      env(x)
    case Rhs.App(f, x) =>
      val func = funcEnv(f.asInstanceOf[Expr.Val.Var].x).asInstanceOf[Rhs.Abs]
      val newEnv = env.updated(func.x, Rhs.Val(x))
      evaluate(func.t, newEnv, funcEnv)
  }

  private def evaluateVal(v: Expr.Val, env: Env): Rhs.Val = v match {
    case l: Expr.Val.Lit => Rhs.Val(l)
    case v: Expr.Val.Var => env(v.x)
  }
