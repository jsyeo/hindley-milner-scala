package nbl

import fastparse.core.Parsed.Success
import nbl.Ast.Value
import org.scalatest._

/**
  * Created by nos on 6/29/16.
  */
class EvaluatorTest extends FunSuite {
  test("Less than expression") {
    val Success(ast, _) = Parser.parse("3-1<1+2")
    assert(Evaluator.eval(ast, Map()) == Value.Boolean(true))
  }

  test("Nested let expression") {
    val Success(ast, _) = Parser.parse("let x = 1+1 in let y = x+1 in 1+x*y")
    assert(Evaluator.eval(ast, Map()) == Value.Integer(7))
  }

  test("Nested if else expression") {
    val Success(ast, _) = Parser.parse("if 1=2 then if 3=4 then 2 else 9 else if 0=1 then 9 else 10")
    val result: Value = Evaluator.eval(ast, Map())
    assert(result == Value.Integer(10))
  }

  test("Function composition") {
    val Success(ast, _) = Parser.parse("let comp = fun f -> fun g -> fun x -> f(g(x)) in let f = fun x -> x*x in let fp = (comp(f))(f) in fp(9)")
    val result: Value = Evaluator.eval(ast, Map())
    assert(result == Value.Integer(6561))
  }

  test("if else with boolean comparison") {
    val Success(ast, _) = Parser.parse("if 3-2=4-3 then 10 else 0")
    val result: Value = Evaluator.eval(ast, Map())
    assert(result == Value.Integer(10))
  }

  test("fibonacci function") {
    val Success(ast, _) = Parser.parse("letrec fib = fun n -> if n<2 then 1 else (fib(n-1))+(fib(n-2)) in fib(10)")
    val result: Value = Evaluator.eval(ast, Map())
    assert(result == Value.Integer(89))
  }
}
