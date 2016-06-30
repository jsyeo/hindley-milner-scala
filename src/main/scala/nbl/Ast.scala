package nbl

import nbl.Ast.Expr.{Fun, Identifier}

/**
  * Created by nos on 6/29/16.
  */
object Ast {

  sealed trait Value

  object Value {

    case class Boolean(value: java.lang.Boolean) extends Value

    case class Integer(value: Int) extends Value

    case class Closure(fun: Fun, env: ClosureEnv) extends Value

  }

  class ClosureEnv(var env: Map[Identifier, Value] = Map())

  sealed trait Expr

  object Expr {

    case class BinOp(operator: Operator, left: Expr, right: Expr) extends Expr

    case class Let(identifier: Identifier, expr: Expr, body: Expr) extends Expr

    case class Identifier(id: String) extends Expr

    case class Integer(value: Int) extends Expr

    case class Boolean(value: java.lang.Boolean) extends Expr

    case class IfElse(conditional: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr

    case class Fun(parameter: Identifier, body: Expr) extends Expr

    case class FunApply(fun: Expr, arg: Expr) extends Expr

    case class LetRec(identifier: Identifier, fun: Fun, body: Expr) extends Expr

  }

  sealed trait Operator

  case object Operator {

    case object Add extends Operator

    case object Minus extends Operator

    case object Multiply extends Operator

    case object Divide extends Operator

    case object Equal extends Operator

    case object LessThan extends Operator

  }

}
