package nbl

import fastparse.all._
import nbl.Main.Expr._
import nbl.Main.Operator.{Add, Divide, Minus, Multiply}

/**
  * Created by nos on 6/27/16.
  */
object Main {

  sealed trait Expr

  object Expr {

    case class BinOp(operator: Operator, left: Expr, right: Expr) extends Expr

    case class Let(identifier: Identifier, expr: Expr, body: Expr) extends Expr

    case class Identifier(variable: String) extends Expr

    sealed trait Value

    case class Integer(value: Int) extends Expr

    case class Boolean(value: java.lang.Boolean) extends Expr

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

    val sp = P(" ")
    val number: P[Expr] = P(CharIn('0' to '9').rep(1).!.map(x => Integer(x.toInt)))
    val keywords = Set("let")
    val identifier: P[Identifier] = P(CharIn('a' to 'z').rep(1).!.filter(!keywords.contains(_)).map(x => Identifier(x)))
    lazy val parens: P[Expr] = P("(" ~/ addSub ~ ")")
    lazy val factor: P[Expr] = P(number | parens | identifier)

    lazy val divMul: P[Expr] = P(factor ~ (CharIn("*/").! ~/ factor).rep).map(transform)
    lazy val addSub: P[Expr] = P(divMul ~ (CharIn("+-").! ~/ divMul).rep).map(transform)

    lazy val let: P[Expr] = P("let" ~/ sp ~ identifier ~ sp ~ "=" ~ sp ~ expr ~ sp ~ "in" ~ sp ~ expr).map(e => {
      Let(e._1, e._2, e._3)
    })
    lazy val expr: P[Expr] = P(addSub | let)

    println(expr.parse("1+1"))
    println(expr.parse("1"))
    println(expr.parse("1+5*2"))
    println(expr.parse("let x = 2 in x*3"))

    def eval(expr: Expr, env: Map[Identifier, ])
  }
}
