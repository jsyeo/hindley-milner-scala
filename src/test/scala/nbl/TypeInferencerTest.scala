package nbl

import fastparse.core.Parsed.Success
import nbl.Type.TypeVar
import org.scalatest.{FunSuite}
import org.scalatest.Matchers._

/**
  * Created by nos on 7/1/16.
  */
class TypeInferencerTest extends FunSuite {

  test("Integer arithmetic inference") {
    val Success(ast, _) = Parser.parse("1+23")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Integer)

    intercept[Error] {
      val Success(ast, _) = Parser.parse("true+25")
      TypeInferencer.infer(ast)
    }
  }

  test("LessThan comparison inference") {
    val Success(ast, _) = Parser.parse("1<23")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Boolean)

    intercept[Error] {
      val Success(ast, _) = Parser.parse("true<25")
      TypeInferencer.infer(ast)
    }
  }

  test("Equal comparison inference") {
    val Success(ast, _) = Parser.parse("23=25")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Boolean)

    intercept[Error] {
      val Success(ast, _) = Parser.parse("true=25")
      TypeInferencer.infer(ast)
    }
  }

  test("If expression inference") {
    val Success(ast, _) = Parser.parse("if true then 123 else 456")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Integer)

    val Success(ast1, _) = Parser.parse("if 1=1 then false else true")
    val typ1 = TypeInferencer.infer(ast1)
    assert(typ1 == Type.Boolean)

    intercept[Error] {
      val Success(ast, _) = Parser.parse("if 1=1 then 123 else true")
      TypeInferencer.infer(ast)
    }

    intercept[Error] {
      val Success(ast, _) = Parser.parse("if 123 then false else true")
      TypeInferencer.infer(ast)
    }
  }

  test("let expression inference") {
    val Success(ast, _) = Parser.parse("let x = 123 in x+4")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Integer)

    val Success(ast1, _) = Parser.parse("let x = true in true=false")
    val typ1 = TypeInferencer.infer(ast1)
    assert(typ1 == Type.Boolean)
  }

  test("identity function inference") {
    val Success(ast, _) = Parser.parse("fun x -> x")
    val typ = TypeInferencer.infer(ast)
    typ shouldBe a [Type.Fun]
    val Type.Fun(param, result) = typ
    param shouldBe a [TypeVar]
    result shouldBe a [TypeVar]
    val TypeVar(id) = param
    val TypeVar(id1) = result
    assert(id == id1)
  }

  test("double function inference") {
    val Success(ast, _) = Parser.parse("fun x -> x*2")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Fun(Type.Integer, Type.Integer))
  }

  test("Multiplication function inference") {
    val Success(ast, _) = Parser.parse("fun x -> fun y -> x*y")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Fun(Type.Integer, Type.Fun(Type.Integer, Type.Integer)))
  }

  test("identity function application") {
    val Success(ast, _) = Parser.parse("(fun x -> x)(23)")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Integer)
  }

  test("double function application") {
    val Success(ast, _) = Parser.parse("(fun x -> x*2)(4)")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Integer)
  }

  test("Curried function inference") {
    val Success(ast, _) = Parser.parse("(fun x -> fun y -> x*y)(2)")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Fun(Type.Integer, Type.Integer))
  }

  test("Multiplication function application") {
    val Success(ast, _) = Parser.parse("((fun x -> fun y -> x*y)(2))(3)")
    val typ = TypeInferencer.infer(ast)
    assert(typ == Type.Integer)
  }

  test("freshTypeVar id increment") {
    val TypeVar(id) = TypeInferencer.freshTypeVar()
    val TypeVar(id1) = TypeInferencer.freshTypeVar()
    assert(id + 1 == id1)
  }

}
