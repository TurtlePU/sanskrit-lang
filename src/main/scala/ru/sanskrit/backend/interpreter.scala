package ru.sanskrit.backend

import ru.sanskrit.common.*
import scala.collection.mutable.Map

object interpreter:
  type Env = Map[Name, Val]


  def run(expr: Expr): Option[Val] = {
    Some(evaluate(expr, Map.empty))  // option is here for future error handling
  }

  private def evaluate(expr: Expr, env: Env): Val = expr match {
    case Let(x, t, v, b) =>
      env += (x -> evaluate(v, env))
      evaluate(b, env)
    case v: Val => evaluateVal(v, env)
    case Sum(a, b) =>
      val valA = evaluateVal(a, env).asInstanceOf[Lit].x
      val valB = evaluateVal(b, env).asInstanceOf[Lit].x
      Lit(valA + valB)
    case Mul(a, b) =>
      val valA = evaluateVal(a, env).asInstanceOf[Lit].x
      val valB = evaluateVal(b, env).asInstanceOf[Lit].x
      Lit(valA * valB)
    case App(f, x) =>
      val func = evaluateVal(f, env).asInstanceOf[Abs]
      var newEnv = env.clone()
      newEnv += (func.x -> evaluateVal(x, env))
      evaluate(func.t, newEnv)

  }

  private def evaluateVal(v: Val, env: Env): Val = v match {
    case l: Lit => l
    case v: Var => env(v.x)
    case a: Abs => a
  }
