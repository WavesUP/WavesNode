package com.wavesplatform.lang.v2.estimator

import cats.Monad
import cats.implicits._
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v2.estimator.EstimatorContext.EvalM
import com.wavesplatform.lang.v2.estimator.EstimatorContext.Lenses._
import monix.eval.Coeval

object ScriptEstimatorV2 extends ScriptEstimator {
  override def apply(
    vars:  Set[String],
    funcs: Map[FunctionHeader, Coeval[Long]],
    expr:  EXPR
  ): Either[ExecutionError, Long] = {
    val v = vars.map((_, (true, const(0)))).toMap
    val f = funcs.mapValues(_.value)
    evalExpr(expr).run(EstimatorContext(v, f)).value._2
  }

  private def evalExpr(t: EXPR): EvalM[Long] =
    t match {
      case LET_BLOCK(let, inner)       => evalLetBlock(let, inner)
      case BLOCK(let: LET, inner)      => evalLetBlock(let, inner)
      case BLOCK(f: FUNC, inner)       => evalFuncBlock(f, inner)
      case REF(str)                    => evalRef(str)
      case _: EVALUATED                => const(1)
      case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
      case GETTER(expr, _)             => evalGetter(expr)
      case FUNCTION_CALL(header, args) => evalFuncCall(header, args)
    }

  private def evalLetBlock(let: LET, inner: EXPR): EvalM[Long] =
    local {
      val letResult = (false, evalExpr(let.value))
      for {
        _ <- modify[EstimatorContext, ExecutionError](lets.modify(_)(_.updated(let.name, letResult)))
        r <- evalExpr(inner)
      } yield r + 5
    }

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM[Long] =
    local {
      for {
        _ <- checkFuncCtx(func)
        _ <- modify[EstimatorContext, ExecutionError](userFuncs.modify(_)(_ + (FunctionHeader.User(func.name) -> func)))
        r <- evalExpr(inner)
      } yield r + 5
    }

  private def checkFuncCtx(func: FUNC): EvalM[Unit] =
    local {
      for {
        _ <- modify[EstimatorContext, ExecutionError](lets.modify(_)(_ ++ func.args.map((_, (true, const(0)))).toMap))
        _ <- evalExpr(func.body)
      } yield ()
    }

  private def evalRef(key: String): EvalM[Long] =
    for {
      ctx <- get
      r <- lets.get(ctx).get(key) match {
        case Some((false, lzy)) => setRefEvaluated(key, lzy)
        case Some((true,  _))   => const(0)
        case None               => raiseError[EstimatorContext, ExecutionError, Long](s"A definition of '$key' not found")
      }
    } yield r + 2

  private def setRefEvaluated(key: String, lzy: EvalM[Long]): EvalM[Long] =
    modify[EstimatorContext, ExecutionError](lets.modify(_)(_.updated(key, (true, lzy))))
      .flatMap(_ => lzy)

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[Long] =
    for {
      condComplexity  <- evalExpr(cond)
      rightComplexity <- evalExpr(ifTrue)
      leftComplexity  <- evalExpr(ifFalse)
    } yield condComplexity + Math.max(leftComplexity, rightComplexity) + 1

  private def evalGetter(expr: EXPR): EvalM[Long] =
    evalExpr(expr).map(_ + 2)

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR]): EvalM[Long] =
    for {
      ctx <- get
      bodyComplexity <- predefFuncs.get(ctx).get(header).map(bodyComplexity => evalFuncArgs(args).map(_ + bodyComplexity))
        .orElse(userFuncs.get(ctx).get(header).map(evalUserFuncCall(_, args)))
        .getOrElse(raiseError(s"function '$header' not found"))
    } yield bodyComplexity

  private def evalUserFuncCall(func: FUNC, args: List[EXPR]): EvalM[Long] = {
    val complexityByArg = args.map(evalExpr)
    val ctxArgs = (func.args zip complexityByArg.map((false, _))).toMap
    for {
      argsComplexity <- complexityByArg.sequence.map(_.sum)
      _              <- modify[EstimatorContext, ExecutionError](lets.modify(_)(_ ++ ctxArgs))
      bodyComplexity <- evalExpr(func.body).map(_ + func.args.size * 5)
    } yield bodyComplexity + argsComplexity
  }

  private def evalFuncArgs(args: List[EXPR]): EvalM[Long] =
    args.traverse(evalExpr).map(_.sum)

  private def const(l: Long): EvalM[Long] =
    Monad[EvalM].pure(l)
}
