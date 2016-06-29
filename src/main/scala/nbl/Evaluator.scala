package nbl

import nbl.Ast.Expr._
import nbl.Ast.Operator.{Add, Divide, Equal, LessThan, Minus, Multiply}
import nbl.Ast.Value.Closure
import nbl.Ast.{ClosureEnv, Expr, Value}

/**
  * Created by nos on 6/29/16.
  */
object Evaluator {

  def evalBinOp(expr: BinOp, env: Map[Identifier, Value]): Value = {
    val (left, right) = (eval(expr.left, env), eval(expr.right, env))
    (left, right) match {
      case ((left: Value.Integer), (right: Value.Integer)) => {
        expr.operator match {
          case Add => Value.Integer(left.value + right.value)
          case Minus => Value.Integer(left.value - right.value)
          case Multiply => Value.Integer(left.value * right.value)
          case Divide => Value.Integer(left.value / right.value)
          case Equal => Value.Boolean(left.value == right.value)
          case LessThan => Value.Boolean(left.value < right.value)
        }
      }
      case (_, _) => throw new Error("Unknown evaluated BinOp operand types: %s %s %s".format(left, expr.operator, right))
    }
  }

  def eval(expr: Expr, env: Map[Identifier, Value]): Value = {
    expr match {
      case (expr: BinOp) => evalBinOp(expr, env)
      case (expr: Let) => eval(expr.body, env.updated(expr.identifier, eval(expr.expr, env)))
      case (expr: Integer) => Value.Integer(expr.value)
      case (expr: Identifier) => {
        env.get(expr).get
      }
      case (expr: Boolean) => Value.Boolean(expr.value)
      case (expr: IfElse) => {
        val Value.Boolean(cond) = eval(expr.conditional, env)
        if (cond)
          eval(expr.thenBranch, env)
        else
          eval(expr.elseBranch, env)
      }
      case (expr: Fun) => {
        Closure(expr, new ClosureEnv(env))
      }
      case (expr: FunApply) => {
        val Closure(Fun(parameter, body), closureEnv) = eval(expr.fun, env)
        val arg = eval(expr.arg, env)
        eval(body, closureEnv.env.updated(parameter, arg))
      }
      case (expr: LetRec) => {
        val functionName = expr.identifier
        val closureEnv = new ClosureEnv(env)
        val closure = Closure(expr.fun, closureEnv)
        closureEnv.env = closureEnv.env.updated(functionName, closure)
        eval(expr.body, closureEnv.env)
      }
    }
  }
}
