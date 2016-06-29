package nbl

import fastparse.all._
import fastparse.core.Parsed.Success
import nbl.Main.Expr
import nbl.Main.Expr._
import nbl.Main.Operator.{Add, _}
import nbl.Main.Value.Closure

/**
  * Created by nos on 6/27/16.
  */
object Main {

  sealed trait Value

  object Value {

    case class Boolean(value: java.lang.Boolean) extends Value

    case class Integer(value: Int) extends Value

    case class Closure(fun: Fun, env: Map[Identifier, Value]) extends Value

  }

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

  }

  sealed trait Operator

  case object Operator {

    case object Add extends Operator

    case object Minus extends Operator

    case object Multiply extends Operator

    case object Divide extends Operator

    case object Equal extends Operator

  }


  def main(args: Array[String]): Unit = {
    def transform(tree: (Expr, Seq[(String, Expr)])): Expr = {
      val (base, ops) = tree
      ops.foldLeft(base) { case (left, (op, right)) => op match {
        case "+" => BinOp(Add, left, right)
        case "-" => BinOp(Minus, left, right)
        case "*" => BinOp(Multiply, left, right)
        case "/" => BinOp(Divide, left, right)
        case "=" => BinOp(Equal, left, right)
      }
      }
    }

    val sp = P(CharIn(" \n"))
    val number: P[Expr] = P(CharIn('0' to '9').rep(1).!.map(x => Integer(x.toInt)))
    val keywords = Set("let", "if", "then", "else", "true", "false", "fun")
    val identifier: P[Identifier] = P(CharIn('a' to 'z').rep(1).!.filter(!keywords.contains(_)).map(x => Identifier(x)))
    val bool: P[Boolean] = P("true" | "false").!.map(b => Boolean(b.equals("true")))

    lazy val parens: P[Expr] = P("(" ~/ expr ~ ")")
    lazy val simple_exp: P[Expr] = P(number | bool | parens | identifier)

    lazy val divMul: P[Expr] = P(simple_exp ~ (CharIn("*/").! ~/ simple_exp).rep).map(transform)
    lazy val addSub: P[Expr] = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(transform)
    lazy val equal: P[Expr] = P(addSub ~ ("=".! ~/ addSub).rep).map(transform)

    lazy val let: P[Expr] = P("let" ~/ sp ~ identifier ~ sp ~ "=" ~ sp ~ expr ~ sp ~ "in" ~ sp ~ expr).map(e =>
      Let(e._1, e._2, e._3))
    lazy val ifElse = P("if" ~/ sp ~ expr ~ sp ~ "then" ~ sp ~ expr ~ sp ~ "else" ~ sp ~ expr).map(e =>
      IfElse(e._1, e._2, e._3))
    lazy val fun = P("fun" ~/ sp ~ identifier ~ sp ~ "->" ~ sp ~ expr).map(e => Fun(e._1, e._2))
    lazy val funApply = P(simple_exp ~ "(" ~ expr ~ ")").map(e => FunApply(e._1, e._2))
    lazy val expr: P[Expr] = P(funApply | fun | let | ifElse | equal)

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
        case (expr: Fun) => Closure(expr, env)
        case (expr: FunApply) => {
          val Closure(Fun(id, body), closureEnv) = eval(expr.fun, env)
          val arg = eval(expr.arg, env)
          eval(body, closureEnv.updated(id, arg))
        }
      }
    }

    val Success(ast, _) = expr.parse("let x = 1+1 in let y = x+1 in 1+x*y")
    assert(eval(ast, Map()) == Value.Integer(7))

    val Success(ast1, _) = expr.parse("if 1=2 then if 3=4 then 2 else 9 else if 0=1 then 9 else 10")
    val result1: Value = eval(ast1, Map())
    assert(result1 == Value.Integer(10))

    val Success(ast2, _) = expr.parse("let comp = fun f -> fun g -> fun x -> f(g(x)) in let f = fun x -> x*x in let fp = (comp(f))(f) in fp(9)")
    val result2: Value = eval(ast2, Map())
    assert(result2 == Value.Integer(6561))
  }
}
