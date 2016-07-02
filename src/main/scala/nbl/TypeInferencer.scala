package nbl

import nbl.Ast.Expr
import nbl.Type.{Fun, TypeVar}
import nbl.Ast.Expr._
import nbl.Ast.Operator._

/**
  * Created by nos on 6/30/16.
  */

sealed trait Type

object Type {

  case class Fun(parameter: Type, result: Type) extends Type

  case object Integer extends Type

  case object Boolean extends Type

  case class TypeVar(id: Int) extends Type

}

object TypeInferencer {

  type Substitution = Map[TypeVar, Type]

  case class Answer(`type`: Type, substitution: Substitution)

  private def applyOneSub(type1: Type, typeVar: TypeVar, type2: Type): Type = type1 match {
    case Type.Integer | Type.Boolean => type1
    case Fun(parameter, result) => Fun(applyOneSub(parameter, typeVar, type2), applyOneSub(result, typeVar, type2))
    case TypeVar(_) => if (type1 == typeVar) type2 else type1
  }

  private def applySubs(typ: Type, subs: Substitution): Type = typ match {
    case Type.Integer | Type.Boolean => typ
    case Fun(parameter, result) => Fun(applySubs(parameter, subs), applySubs(result, subs))
    case typeVar@TypeVar(_) => subs.get(typeVar).getOrElse(typeVar)
  }

  private def extendSubs(subs: Substitution, typeVar: TypeVar, typ: Type): Substitution = {
    subs.map { case (lhs, rhs) => (lhs, applyOneSub(rhs, typeVar, typ)) }.updated(typeVar, typ)
  }

  private def unify(type1: Type, type2: Type, subs: Substitution, expr: Expr): Substitution = {
    val subed1 = applySubs(type1, subs)
    val subed2 = applySubs(type2, subs)
    (subed1, subed2) match {
      case (_, _) if (subed1 == subed2) => subs
      case (type1: TypeVar, _) => extendSubs(subs, type1, subed2)
      case (_, type2: TypeVar) => extendSubs(subs, type2, subed1)
      case (type1: Fun, type2: Fun) =>
        val unifiedSubs = unify(type1.parameter, type2.parameter, subs, expr)
        unify(type1.result, type2.result, unifiedSubs, expr)
      case _ => throw new Error(s"Unable to unify $subed1 and $subed2 in the $expr expression")
    }
  }

  def infer(expr: Expr): Type = {
    val freshTypeVar = (() => {
      // Hidden side effect! YAY!
      var counter = 0
      () => {
        val res = counter
        counter += 1
        TypeVar(res)
      }
    }) ()

    def inferBinOp(binOp: BinOp, typeEnv: Map[Expr, Type], subs: Substitution): Answer = {
      val Answer(leftType, leftSubs) = inner(binOp.left, typeEnv, subs)
      binOp.operator match {
        case Add | Minus | Multiply | Divide =>
          val unifiedLeftSubs = unify(leftType, Type.Integer, leftSubs, binOp.left)
          val Answer(rightType, rightSubs) = inner(binOp.right, typeEnv, unifiedLeftSubs)
          val unifiedRightSubs = unify(rightType, Type.Integer, rightSubs, binOp.right)
          Answer(Type.Integer, unifiedRightSubs)
        case LessThan =>
          val unifiedLeftSubs = unify(leftType, Type.Integer, leftSubs, binOp.left)
          val Answer(rightType, rightSubs) = inner(binOp.right, typeEnv, unifiedLeftSubs)
          val unifiedRightSubs = unify(rightType, Type.Integer, rightSubs, binOp.right)
          Answer(Type.Boolean, unifiedRightSubs)
        case Equal =>
          val Answer(rightType, rightSubs) = inner(binOp.right, typeEnv, leftSubs)
          val unifiedSubs = unify(leftType, rightType, rightSubs, binOp)
          Answer(Type.Boolean, unifiedSubs)
      }
    }

    def inner(expr: Expr, typeEnv: Map[Expr, Type], subs: Substitution): Answer = {
      expr match {
        case (expr: Integer) => Answer(Type.Integer, subs)
        case (expr: Boolean) => Answer(Type.Boolean, subs)
        case (expr: BinOp) => inferBinOp(expr, typeEnv, subs)
        case (expr: IfElse) =>
          val Answer(condType, condSubs) = inner(expr.conditional, typeEnv, subs)
          val unifiedCondSubs = unify(condType, Type.Boolean, condSubs, expr.conditional)
          val Answer(thenType, thenSubs) = inner(expr.thenBranch, typeEnv, unifiedCondSubs)
          val Answer(elseType, elseSubs) = inner(expr.elseBranch, typeEnv, thenSubs)
          val unifiedSubs = unify(thenType, elseType, elseSubs, expr)
          Answer(elseType, unifiedSubs)
        case (expr: Identifier) => Answer(typeEnv.get(expr).get, subs)
        case (expr: Let) =>
          val Answer(boundType, boundSubs) = inner(expr.expr, typeEnv, subs)
          inner(expr.body, typeEnv.updated(expr.identifier, boundType), boundSubs)
        case (expr: Expr.Fun) =>
          val paramType = freshTypeVar()
          val Answer(resultType, resultSubs) = inner(expr.body, typeEnv.updated(expr.parameter, paramType), subs)
          Answer(Type.Fun(paramType, resultType), resultSubs)
        case (expr: FunApply) =>
          val resultType = freshTypeVar()
          val Answer(funType, funSubs) = inner(expr.fun, typeEnv, subs)
          val Answer(argType, argSubs) = inner(expr.arg, typeEnv, funSubs)
          val unifiedSubs = unify(funType, Fun(argType, resultType), argSubs, expr)
          Answer(resultType, unifiedSubs)
        case (expr: LetRec) =>
          val resultType = freshTypeVar()
          val paramType = freshTypeVar()
          val bodyTypeEnv = typeEnv.updated(expr.identifier, Type.Fun(paramType, resultType))
          val Answer(funBodyType, funBodySubs) = inner(expr.fun.body, bodyTypeEnv.updated(expr.fun.parameter, paramType), subs)
          val unifiedSubs = unify(funBodyType, resultType, funBodySubs, expr.fun)
          inner(expr.body, bodyTypeEnv, unifiedSubs)
      }
    }

    val Answer(typ, subs) = inner(expr, Map(), Map())
    applySubs(typ, subs)
  }
}
