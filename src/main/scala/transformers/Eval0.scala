package transformers

import transformers.Eval.Exp._
import transformers.Eval.Value.{FunVal, IntVal}
import transformers.Eval._

object Eval0 {

  def main(args: Array[String]): Unit = {
    println(s"$ExampleExp = ${eval0(Map.empty, ExampleExp)}")
  }

  def eval0(env: Env, exp: Exp): Value = exp match {
    case Lit(i) =>
      IntVal(i)

    case Var(n) =>
      env(n)

    case Plus(e1, e2) =>
      val IntVal(i1) = eval0(env, e1)
      val IntVal(i2) = eval0(env, e2)
      IntVal(i1 + i2)

    case Abs(n, e) =>
      FunVal(env, n, e)

    case App(e1, e2) => {
      val FunVal(e, n, exp) = eval0(env, e1)
      val val2 = eval0(env, e2)
      eval0(e + (n -> val2), exp)
    }
  }
}
