package transformers

import transformers.Eval.Exp._
import transformers.Eval.Value._
import transformers.Eval.{Value, _}

import scalaz.Scalaz._
import scalaz.{Plus => _, _}

object Eval2 {

  type Eval2[A] = EitherT[Identity, String, A]

  def main(args: Array[String]): Unit = {
    println(s"$ExampleExp = ${eval2(Map.empty, ExampleExp).run.value}")
  }

  def eval2(env: Env, exp: Exp): Eval2[Value] =
    exp match {
      case Lit(i) =>
        intVal(i).point[Eval2]

      case Var(n) =>
        EitherT {
          env
            .get(n)
            .fold(s"Name '$n' is undefined.".left[Value])(_.right[String])
            .point[Identity]
        }

      case Plus(e1, e2) =>
        for {
          IntVal(i) <- eval2(env, e1)
          IntVal(j) <- eval2(env, e2)
        } yield IntVal(i + j)

      case Abs(n, e) =>
        funVal(env, n, e).point[Eval2]

      case App(e1, e2) =>
        for {
          FunVal(e, n, exp) <- eval2(env, e1)
          val2 <- eval2(env, e2)
          res <- eval2(e + (n -> val2), exp)
        } yield res
    }
}
