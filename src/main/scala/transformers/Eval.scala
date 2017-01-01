package transformers

object Eval {

  type Name = String
  type Env = Map[Name, Value]

  sealed trait Exp

  object Exp {

    val ExampleExp = Plus(Lit(12), App(Abs("x", Var("x")), Plus(Lit(4), Lit(2))))

    case class Lit(value: Int) extends Exp

    case class Var(name: Name) extends Exp

    case class Plus(exp1: Exp, exp2: Exp) extends Exp

    case class Abs(name: Name, exp: Exp) extends Exp

    case class App(exp1: Exp, exp2: Exp) extends Exp

  }

  sealed trait Value

  object Value {

    case class IntVal(value: Int) extends Value

    case class FunVal(env: Env, name: Name, exp: Exp) extends Value

    def intVal(value: Int): Value = IntVal(value)

    def funVal(env: Env, name: Name, exp: Exp): Value = FunVal(env, name, exp)

  }
}
