package nbl

import fastparse.core.Parsed.{Failure, Success}
import nbl.Ast.Expr._
import nbl.Ast.Operator.{Add, Equal, Multiply}
import org.scalatest.FunSuite

/**
  * Created by nos on 6/29/16.
  */
class ParserTest extends FunSuite {
  test("Binary operation precedence") {
    val Success(ast, _) = Parser.parse("4+3*2")
    assert(ast == BinOp(Add, Integer(4), BinOp(Multiply, Integer(3), Integer(2))))
  }


  test("Add operation with parens") {
    val Success(ast, _) = Parser.parse("1+(4+5)+(1+1)")
    assert(ast == BinOp(Add, BinOp(Add, Integer(1), BinOp(Add,Integer(4),Integer(5))),
                             BinOp(Add,Integer(1),Integer(1))))
  }

  test("Function application with addition") {
    val Success(ast, _) = Parser.parse("1+f(2)")
    assert(ast == BinOp(Add, Integer(1), FunApply(Identifier("f"), Integer(2))))

    val Success(ast2, _) = Parser.parse("f(2)+1")
    assert(ast2 == BinOp(Add, FunApply(Identifier("f"), Integer(2)), Integer(1)))
  }

  test("Add result of two function applications") {
    val Success(ast, _) = Parser.parse("f(1)+f(2)")
    assert(ast == BinOp(Add, FunApply(Identifier("f"), Integer(1)), FunApply(Identifier("f"), Integer(2))))
  }

  test("if expression") {
    val Success(ast, _) = Parser.parse("if x=0 then 42 else 88")
    assert(ast == IfElse(BinOp(Equal, Identifier("x"), Integer(0)), Integer(42), Integer(88)))
  }

  test("let expression") {
    val Success(ast, _) = Parser.parse("let x = 12 in x*2")
    assert(Let(Identifier("x"), Integer(12), BinOp(Multiply, Identifier("x"), Integer(2))) == ast)
  }

  test("funtion expression") {
    val Success(ast, _) = Parser.parse("let f = fun x -> x*x in 3*2")
    assert(Let(Identifier("f"), Fun(Identifier("x"), BinOp(Multiply, Identifier("x"), Identifier("x"))), BinOp(Multiply, Integer(3), Integer(2))) == ast)
  }

  test("funtion definition and application expression") {
    val Success(ast, _) = Parser.parse("let f = fun x -> x*x in f(3)*f(2)")
    assert(Let(Identifier("f"), Fun(Identifier("x"), BinOp(Multiply, Identifier("x"), Identifier("x"))),
                                BinOp(Multiply, FunApply(Identifier("f"), Integer(3)), FunApply(Identifier("f"), Integer(2)))) == ast)
  }

  test("letrec expression") {
    val Success(ast, _) = Parser.parse("letrec f = fun x -> f(x) in f(1)")
    assert(LetRec(Identifier("f"), Fun(Identifier("x"), FunApply(Identifier("f"), Identifier("x"))), FunApply(Identifier("f"), Integer(1))) == ast)
  }

  test("Function application with binary operation as argument") {
    val parsed = Parser.parse("f(1+2)")
    val Success(ast, _) = parsed
    assert(ast == FunApply(Identifier("f"), BinOp(Add, Integer(1), Integer(2))))
  }

  test("Function composition") {
    val parsed = Parser.parse("f(g(x))")
    val Success(ast, _) = parsed
    assert(ast == FunApply(Identifier("f"), FunApply(Identifier("g"), Identifier("x"))))
  }
}
