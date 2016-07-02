package nbl

import nbl.Ast.Expr

import scala.tools.jline.console.ConsoleReader

/**
  * Created by nos on 7/1/16.
  */
object Main {
  def main(args: Array[String]): Unit = {
    val consoleReader = new ConsoleReader
    def read(): Expr = Parser.parse(consoleReader.readLine("> ")).get.value
    def infer(expr: Expr): Type = TypeInferencer.infer(expr)
    def print(typ: Type): Unit = {
      def inner(typ: Type): String = {
        typ match {
          case Type.Integer => "int"
          case Type.Boolean => "bool"
          case Type.TypeVar(id) => (id + 97).toChar.toString
          case Type.Fun(parameter, result) =>
            val param = inner(parameter)
            val res = inner(result)
            s"($param -> $res)"
        }
      }
      println(inner(typ))
    }

    implicit def toPipe[T](x: T) = new {
      def |>[U](f: T => U) = f(x)
    }

    def loop(): Unit = { read |> infer |> print ; loop }

    loop()
  }
}
