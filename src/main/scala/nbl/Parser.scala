package nbl

import fastparse.all._
import fastparse.parsers.Intrinsics.CharIn
import nbl.Ast.Expr
import nbl.Ast.Expr._
import nbl.Ast.Operator.{Equal, LessThan, Add, Minus, Multiply, Divide}

/**
  * Created by nos on 6/29/16.
  */
object Parser {
  private def transform(tree: (Expr, Seq[(String, Expr)])): Expr = {
    val (base, ops) = tree
    ops.foldLeft(base) { case (left, (op, right)) => op match {
      case "+" => BinOp(Add, left, right)
      case "-" => BinOp(Minus, left, right)
      case "*" => BinOp(Multiply, left, right)
      case "/" => BinOp(Divide, left, right)
      case "=" => BinOp(Equal, left, right)
      case "<" => BinOp(LessThan, left, right)
    }
    }
  }

  val sp = P(CharIn(" \n"))
  val number: P[Expr] = P(CharIn('0' to '9').rep(1).!.map(x => Integer(x.toInt)))
  val keywords = Set("let", "if", "then", "else", "true", "false", "fun")
  val identifier: P[Identifier] = P(CharIn('a' to 'z').rep(1).!.filter(!keywords.contains(_)).map(x => Identifier(x)))
  val bool: P[Boolean] = P("true" | "false").!.map(b => Boolean(b.equals("true")))

  lazy val parens: P[Expr] = P("(" ~/ expr ~/ ")")
  lazy val simple_exp: P[Expr] = P(number | bool | identifier | parens)

  lazy val funApply = P(simple_exp ~/ ("(" ~/ expr ~/ ")").?).map {
    case (e, Some(e1)) => FunApply(e, e1)
    case (e, None) => e
  }
  lazy val divMul: P[Expr] = P(funApply ~/ (CharIn("*/").! ~/ funApply).rep).map(transform)
  lazy val addSub: P[Expr] = P(divMul ~/ (CharIn("+-").! ~/ divMul).rep).map(transform)
  lazy val ltEqual: P[Expr] = P(addSub ~/ (CharIn("<=").! ~/ addSub).rep).map(transform)

  lazy val ifElse = P("if" ~/ sp ~ expr ~ sp ~ "then" ~ sp ~ expr ~ sp ~ "else" ~ sp ~ expr).map(e =>
    IfElse(e._1, e._2, e._3))
  lazy val let: P[Expr] = P("let" ~ sp ~ identifier ~ sp ~ "=" ~ sp ~ expr ~ sp ~ "in" ~ sp ~ expr).map(e =>
    Let(e._1, e._2, e._3))
  lazy val fun = P("fun" ~/ sp ~ identifier ~ sp ~ "->" ~ sp ~ expr).map(e => Fun(e._1, e._2))
  lazy val letRec: P[Expr] = P("letrec" ~/ sp ~ identifier ~ sp ~ "=" ~ sp ~ fun ~ sp ~ "in" ~ sp ~ expr).map(e =>
    LetRec(e._1, e._2, e._3))
  lazy val expr: P[Expr] = P(ifElse | let | letRec | fun | ltEqual)

  def parse(s: String): Parsed[Expr] = expr.parse(s)
}
