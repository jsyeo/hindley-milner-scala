package nbl

import fastparse.all._
import fastparse.core.Parsed.Success
import nbl.Main.Expr._
import nbl.Main.Operator.{Add, _}

/**
  * Created by nos on 6/27/16.
  */
object Main {

  sealed trait Result

  object Result {

    case class Boolean(value: Boolean) extends Result

    case class Integer(value: Int) extends Result

  }

  sealed trait Expr

  object Expr {

    case class BinOp(operator: Operator, left: Expr, right: Expr) extends Expr

    case class Let(identifier: Identifier, expr: Expr, body: Expr) extends Expr

    case class Identifier(id: String) extends Expr

    sealed trait Value

    case class Integer(value: Int) extends Expr

    case class Boolean(value: java.lang.Boolean) extends Expr

    case class IfElse(conditional: Expr, thenBranch: Expr, elseBranch: Expr) extends Expr

  }

  sealed trait Operator

  case object Operator {

    case object Add extends Operator

    case object Minus extends Operator

    case object Multiply extends Operator

    case object Divide extends Operator

  }


  def main(args: Array[String]): Unit = {
    def transform(tree: (Expr, Seq[(String, Expr)])): Expr = {
      val (base, ops) = tree
      ops.foldLeft(base) { case (left, (op, right)) => op match {
        case "+" => BinOp(Add, left, right)
        case "-" => BinOp(Minus, left, right)
        case "*" => BinOp(Multiply, left, right)
        case "/" => BinOp(Divide, left, right)
      }
      }
    }

    val sp = P(CharIn(" \n"))
    val number: P[Expr] = P(CharIn('0' to '9').rep(1).!.map(x => Integer(x.toInt)))
    val keywords = Set("let", "if", "then", "else", "true", "false")
    val identifier: P[Identifier] = P(CharIn('a' to 'z').rep(1).!.filter(!keywords.contains(_)).map(x => Identifier(x)))
    val bool: P[Boolean] = P("true" | "false").!.map(b => Boolean(b.equals("true")))
    lazy val parens: P[Expr] = P("(" ~/ simple_exp ~ ")")
    lazy val simple_exp: P[Expr] = P(number | bool | parens | identifier)

    lazy val divMul: P[Expr] = P(simple_exp ~ (CharIn("*/").! ~/ simple_exp).rep).map(transform)
    lazy val addSub: P[Expr] = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(transform)

    lazy val let: P[Expr] = P("let" ~/ sp ~ identifier ~ sp ~ "=" ~ sp ~ expr ~ sp ~ "in" ~ sp ~ expr).map(e =>
      Let(e._1, e._2, e._3))
    lazy val ifElse = P("if" ~/ sp ~ expr ~ sp ~ "then" ~ sp ~ expr ~ sp ~ "else" ~ sp ~ expr).map(e =>
      IfElse(e._1, e._2, e._3))
    lazy val expr: P[Expr] = P(addSub | let | ifElse)

    def evalBinOp(expr: BinOp, env: Map[Identifier, Expr]): Expr = {
      val (left, right) = (eval(expr.left, env), eval(expr.right, env))
      (left, right) match {
        case ((left: Integer), (right: Integer)) => {
          expr.operator match {
            case Add => Integer(left.value + right.value)
            case Minus => Integer(left.value - right.value)
            case Multiply => Integer(left.value * right.value)
            case Divide => Integer(left.value / right.value)
          }
        }
        case (_, _) => throw new Error("Unknown evaluated BinOp operand types: %s %s %s".format(left, expr.operator, right))
      }
    }

    def eval(expr: Expr, env: Map[Identifier, Expr]): Expr = {
      expr match {
        case (expr: BinOp) => evalBinOp(expr, env)
        case (expr: Let) => eval(expr.body, env.updated(expr.identifier, eval(expr.expr, env)))
        case (expr: Integer) => expr
        case (expr: Identifier) => env.get(expr).get
        case (expr: Boolean) => expr
        case (expr: IfElse) => {
          val Boolean(cond) = eval(expr.conditional, env)
          if (cond)
            eval(expr.thenBranch, env)
          else
            eval(expr.elseBranch, env)
        }
      }
    }

    val Success(ast, _) = expr.parse("let x = 1+1 in let y = x+1 in 1+x*y")
    assert(eval(ast, Map()) == Integer(7))

    val Success(ast1, _) = expr.parse("if false then if false then 2 else 9 else if false then 9 else 10")
    val eval1: Expr = eval(ast1, Map())
    assert(eval1 == Integer(10))
  }
}
