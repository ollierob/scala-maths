package net.ollie.maths.methods

import scala.Some
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import net.ollie.maths._
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 19/01/14.
 */
object Series {

    def apply(left: Expression, right: Expression): Expression = (left, right) match {
        case _ if left.isEmpty => right
        case _ if right.isEmpty => left
        case (_, s: Series[_]) => s :+ left
        case (s: Series[_], _) => s + right
        case _ => new Series(Seq(left, right))
    }

    def apply(terms: Iterable[Expression]): Expression = terms.filterNot(_.isEmpty) match {
        case Nil => Zero
        case expr :: Nil => expr
        case otherwise => new Series(otherwise.toSeq)
    }

    def apply(f: (Integer) => Expression, start: Integer, end: Integer): Expression = {
        if (end < start) Zero
        else new FiniteIncrementalSum(f, start, end)
    }

    def apply(f: (Natural) => Real, start: Natural): Real = new InfiniteSum(f, start)

    def apply[N <: Integer](f: (N) => Real, over: Seq[N]) = if (over.isEmpty) Zero else new SumOver(f, over)

}

class Series[+T <: Expression] protected(val terms: Seq[T])
extends Aggregate {

    protected[this] def apply(expressions: Seq[Expression]) = Series(expressions)

    def toConstant: Option[Number] = {
        val mapped: Seq[Option[Number]] = terms.map(_.toConstant)
        val zero: Option[Number] = Some(Zero)
        mapped.foldLeft(zero)(add)
    }

    private def add(result: Option[Number], current: Option[Number]): Option[Number] = {
        if (!result.isDefined || !current.isDefined) return None
        val n = result.get ?+ current.get
        if (n.isDefined) n
        else current.get ?+ result.get
    }

    override def +(that: Expression) = that match {
        case s: Series[_] => Series(terms ++: s.terms)
        case _ => Series(terms :+ that)
    }

    def :+(that: Expression) = that match {
        case s: Series[_] => Series(s.terms ++: terms)
        case _ => Series(that +: terms)
    }

    override def df(x: Variable) = Series(terms.map(_.df(x)))

    def isEmpty = terms.forall(_.isEmpty)

    override def toString = terms.mkString("(", " + ", ")")

    override def hashCode = terms.hashCode

}

private class FiniteIncrementalSum(f: (Integer) => Expression, start: Integer, end: Integer)
        extends Expression {

    private val size = (end - start).toInt.get

    private lazy val series: Expression = {
        val terms: mutable.ArrayBuffer[Expression] = new mutable.ArrayBuffer[Expression](size)
        for (i <- 0 to size) terms += f(start + i)
        Series(terms)
    }

    def df(x: Variable) = series.df(x)

    def replace(variables: Map[Variable, Expression]) = series.replace(variables)

    def toConstant = series.toConstant

    def variables = series.variables

    def isEmpty = series.isEmpty

    override def toString = s"Σ($start:$end)($f)"

}

private class InfiniteSum(f: Natural => Real, start: Natural)
        extends Real
        with IterativelyEvaluated {

    def isEmpty = false

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator {

        val terms: ListBuffer[Real] = new ListBuffer[Real]()

        def next(nth: Natural, precision: Precision) = {
            val n: Integer = nth + start
            terms += f(n)
            terms.map(_.approximatelyEvaluate(precision)).sum
        }

    }

}

private class SumOver[N <: Integer](f: N => Real, over: Seq[N])
        extends Real {

    private val series: Iterable[Real] = {
        val terms = new ArrayBuffer[Real](over.size)
        for (i <- over) terms += f(i)
        terms
    }

    protected[this] def eval(precision: Precision) = series.sum.evaluate(precision)

    def isEmpty = over.isEmpty || series.forall(_.isEmpty)

    override def toString = s"Σ(over $over)($f)"

}