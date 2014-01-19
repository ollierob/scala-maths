package net.ollie.maths.methods

import scala.Some
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.RoundingMode

import net.ollie.maths._
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 19/01/14.
 */
object Series {

    def apply(left: Expression, right: Expression): Expression = if (left.isEmpty) right else if (right.isEmpty) left else new Series(Seq(left, right))

    def apply(terms: Iterable[Expression]): Expression = terms.filterNot(_.isEmpty) match {
        case Nil => Zero
        case expr :: Nil => expr
        case otherwise => new Series(otherwise.toSeq)
    }

    def apply(f: (IntegerNumber) => Expression, start: IntegerNumber, end: IntegerNumber): Expression = {
        if (end < start) Zero
        else new FiniteSum(f, start, end)
    }

    def apply(f: NaturalNumber => RealNumber, start: NaturalNumber): RealNumber = new InfiniteSum(f, start)

}

class Series[+T <: Expression](val terms: Seq[T])
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

    override def df(x: Variable) = Series(terms.map(_.df(x)))

    def isEmpty = terms.forall(_.isEmpty)

    override def toString = terms.mkString("Σ(", " + ", ")")

    override def hashCode = terms.hashCode

}

class FiniteSum(f: (IntegerNumber) => Expression, start: IntegerNumber, end: IntegerNumber)
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

class InfiniteSum(f: NaturalNumber => RealNumber, start: NaturalNumber)
        extends RealNumber
        with IterativelyEvaluated {

    def isEmpty = false;

    def evaluationIterator = new EvaluationIterator() {

        val terms: ListBuffer[RealNumber] = new ListBuffer[RealNumber]()

        def next(nth: NaturalNumber, precision: Precision)(implicit mode: RoundingMode.RoundingMode) = {
            val n: NaturalNumber = nth + start
            terms += f(n)
            terms.map(_.approximatelyEvaluate(precision)).sum
        }

    }

}