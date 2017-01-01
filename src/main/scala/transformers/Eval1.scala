package transformers

import transformers.Eval.Exp._
import transformers.Eval.Value._
import transformers.Eval.{Value, _}

import scalaz.Scalaz._

object Eval1 {

  type Eval1[A] = Identity[A]

  def main(args: Array[String]): Unit = {
    println(s"$ExampleExp = ${eval1(Map.empty, ExampleExp).value}")
  }

  def eval1(env: Env, exp: Exp): Eval1[Value] =
    exp match {
      case Lit(i) =>
        intVal(i).point[Eval1]

      case Var(n) =>
        env(n).point[Eval1]

      case Plus(e1, e2) =>
        for {
          val1 <- eval1(env, e1)
          IntVal(i) = val1
          val2 <- eval1(env, e2)
          IntVal(j) = val2
        } yield IntVal(i + j)

      case Abs(n, e) =>
        funVal(env, n, e).point[Eval1]

      case App(e1, e2) => for {
        val1 <- eval1(env, e1)
        FunVal(e, n, exp) = val1
        val2 <- eval1(env, e2)
        res <- eval1(e + (n -> val2), exp)
      } yield res
    }
}
