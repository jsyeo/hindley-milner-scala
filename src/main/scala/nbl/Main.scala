package nbl

import fastparse.core.ParseError
import nbl.Ast.Value.{Boolean, Closure, Integer}
import nbl.Ast.{Expr, Value}
import nbl.Type.TypeVar

import scala.tools.jline.console.ConsoleReader

/**
  * Created by nos on 7/1/16.
  */
object Main {

  val consoleReader = new ConsoleReader

  def main(args: Array[String]): Unit = {

    implicit def toPipe[T](x: T) = new {
      def |>[U](f: T => U) = f(x)
    }

    def loop(): Unit = {
      read |> infer |> eval |> print;
      loop
    }

    loop()
  }

  def read(): Option[Expr] = {
    val line: String = consoleReader.readLine("nbl> ")
    if (line == null) System.exit(0)
    if (line.trim.isEmpty) None
    else {
      try {
        Some(Parser.parse(line).get.value)
      } catch {
        case error: ParseError =>
          println(error.getMessage)
          None
      }
    }
  }

  def infer(expr: Option[Expr]): Option[Expr] = {
    try {
      expr.map(TypeInferencer.infer(_))
      expr
    } catch {
      case error: Error =>
        println(error.getMessage())
        None
    }
  }

  def eval(expr: Option[Expr]): Option[(Type, Value)] = {
    expr.map(expr => {
      val value = Evaluator.eval(expr)
      value match {
        case Boolean(_) => (Type.Boolean, value)
        case Integer(_) => (Type.Integer, value)
        case Closure(fun, _) => (TypeInferencer.infer(expr), value)
      }
    })
  }

  def print(typeValuePair: Option[(Type, Value)]): Unit = {
    def typeToString(typ: Type): String = {
      def inner(typ: Type, typeVars: Map[TypeVar, Int], counter: Int): (String, Map[TypeVar, Int], Int) = {
        typ match {
          case Type.Integer => ("int", typeVars, counter)
          case Type.Boolean => ("bool", typeVars, counter)
          case typ@Type.TypeVar(_) =>
            val typeVar: String = "'" + (typeVars.getOrElse(typ, counter) + 97).toChar.toString
            (typeVar, typeVars.updated(typ, counter), counter + 1)
          case Type.Fun(parameter, result) =>
            val (param, paramTypeVars, paramCounter) = inner(parameter, typeVars, counter)
            val (res, resTypeVars, resCounter) = inner(result, paramTypeVars, paramCounter)
            (s"($param -> $res)", resTypeVars, resCounter)
        }
      }
      inner(typ, Map(), 0)._1
    }
    def printTypeValuePair(typeValuePair: (Type, Value)) = {
      typeValuePair match {
        case (_, Boolean(value)) => println(value)
        case (_, Integer(value)) => println(value)
        case _ =>
      }
      println(":: " + typeToString(typeValuePair._1))
    }
    typeValuePair.map(printTypeValuePair(_))
  }

}
