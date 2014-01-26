package net.ollie.maths.numbers

import scala.Some
import scala.collection.mutable

import net.ollie.maths._
import net.ollie.maths.methods.{ApproximatelyEvaluated, Product, Series}
import net.ollie.maths.numbers.real.{Massive, PowerTower, RealPower}

/**
 * Created by Ollie on 01/01/14.
 */
trait Real
        extends Number
        with Evaluable
        with Ordered[Real] {

    final type System = Real

    override def replace(variables: Map[Variable, Expression]) = this

    override def unary_-(): Real = Real.negate(this)

    override def df(x: Variable): Real with Empty = Zero

    def inverse: Real = Real.inverse(this)

    def abs = Real.abs(this)

    def +(that: Real): Real = this ?+ that match {
        case Some(n) => n
        case _ => that ?+ this match {
            case Some(m) => m
            case _ => RealSeries(this, that)
        }
    }

    def ?+(that: Real): Option[Real] = that match {
        case Zero => Some(this)
        case _ => None
    }

    def ?+?(that: Real): Option[Real] = this ?+ that match {
        case Some(m) => Some(m)
        case _ => that ?+ this match {
            case Some(m) => Some(m)
            case _ => None
        }
    }

    def ?+(that: Number) = that match {
        case re: Real => Some(this + re)
        case _ => None
    }

    def -(that: Real): Real = this + (-that)

    def *(that: Real): Real = {
        this ?* that match {
            case Some(n) => n
            case _ => that ?* this match {
                case Some(m) => m
                case _ => RealProduct(this, that)
            }
        }
    }

    def ?*(that: Real): Option[Real] = that match {
        case Zero => Some(Zero)
        case One => Some(this)
        case _ => None
    }

    /**
     * Shortcut for this ?* that and that ?* this. Note that real multiplication is commutative.
     * @param that
     * @return
     */
    def ?*?(that: Real): Option[Real] = this ?* that match {
        case Some(m) => Some(m)
        case _ => that ?* this match {
            case Some(m) => Some(m)
            case _ => None
        }
    }

    /**
     *
     * @param that
     * @return
     */
    override def ?*(that: Number)(leftToRight: Boolean) = that match {
        case re: Real => Some(this * re)
        case _ => None
    }

    def /(that: Real): Real = this * that.inverse

    def ^(that: Integer): Real = Real.pow(this, that)

    def ?^(that: Number): Option[Number] = that match {
        case int: Integer => Some(this ^ int)
        case re: Real if this.isStrictlyPositive => Some(this.abs ^ re)
        case _ => None
    }

    def squared: PositiveReal = (this ^ 2).abs

    def compare(that: Real): Int = {
        if (this eq that) 0
        else this.tryCompareTo(that) match {
            case Some(i) => i
            case _ => that.tryCompareTo(this) match {
                case Some(j) => -j
                case _ => this.evaluate(SinglePrecision).compare(that.evaluate(SinglePrecision))
            }
        }
    }

    def isStrictlyPositive: Boolean = Zero < this

    protected def tryCompareTo(that: Real): Option[Int] = None

    override def equals(number: Number) = number match {
        case re: Real => this.equals(re)
        case _ => super.equals(number)
    }

    def equals(that: Real): Boolean = super.equals(that) || (this ?== that match {
        case Some(b) => b
        case _ => that ?== this match {
            case Some(b) => b
            case _ => this.evaluate(SinglePrecision) == that.evaluate(SinglePrecision)
        }
    })

    def ?==(that: Real): Option[Boolean] = None

}

object Real {

    implicit def apply(int: Int): Real = Integer(int)

    def apply(value: BigDecimal): Real = if (value == 0) Zero else new ExactReal(value)

    def negate(re: Real) = if (re.isEmpty) Zero else new NegatedReal(re)

    def inverse(re: Real) = new InverseReal(re)

    def abs(re: Real): PositiveReal = re match {
        case Zero => Zero
        case p: PositiveReal => p
        case _ => new AbsReal(re)
    }

    def pow(base: Real, power: Integer): Real = RealPower(base, power)

    implicit object NumberToReal
            extends NumberIdentityArithmetic[Real] {

        def convert(from: Number): Option[Real] = from match {
            case re: Real => Some(re)
            case _ => None
        }

        override def toString = "Number -> Real"

    }

    implicit object RealArithmetic
            extends AdditionArithmetic[Real, Real, Real]
            with MultiplicationArithmetic[Real, Real, Real]
            with TetrationArithmetic[Real, Real, Massive]
            with scala.math.Numeric[Real] {

        def add(x: Real, y: Real) = plus(x, y)

        def plus(x: Real, y: Real) = x + y

        def minus(x: Real, y: Real) = x - y

        def times(x: Real, y: Real) = ???

        def negate(x: Real) = -x

        def fromInt(x: Int) = Real(x)

        def toInt(x: Real) = x.evaluate(IntegerPrecision).toInt

        def toLong(x: Real) = x.evaluate(IntegerPrecision).toLong

        def toFloat(x: Real) = x.evaluate(SinglePrecision).toFloat

        def toDouble(x: Real) = x.evaluate(DoublePrecision).toDouble

        def compare(x: Real, y: Real) = x.compareTo(y)

        def multiply(left: Real, right: Real) = left * right

        def tetrate(base: Real, tower: Real) = PowerTower(base, tower)

    }

}

class ExactReal(val of: BigDecimal)
        extends AnyRef
        with Real
        with ApproximatelyEvaluated {

    override def ?+(that: Real) = that match {
        case re: ExactReal => Some(Real(this.of + re.of))
        case _ => super.?+(that)
    }

    override def ?*(that: Real) = that match {
        case re: ExactReal => Some(Real(this.of * re.of))
        case _ => super.?*(that)
    }

    def isEmpty = of == 0

    override def approx(precision: Precision) = precision(of)

    override def toString = of.toString

}

class NegatedReal(val of: Real)
        extends Real {

    protected[this] def eval(precision: Precision) = -(of.evaluate(precision))

    override def variables = super[Real].variables

    override def unary_-() = of

    override def toConstant = Some(this)

    def isEmpty = of.isEmpty

    override def toString = s"-($of)"

    override def hashCode = -of.hashCode

}

class InverseReal(val of: Real)
        extends Real
        with ApproximatelyEvaluated {

    require(!of.isEmpty)

    override def inverse = of

    def isEmpty = false

    override def ?*(that: Real) = that match {
        case real: Real if of == real => Some(One)
        case _ => super.?*(that)
    }

    override def toString = s"1/$of"

    override def approx(precision: Precision) = 1 / of.evaluate(precision)

}

class AbsReal(val of: Real)
        extends PositiveReal {

    protected[this] def eval(precision: Precision) = of.evaluate(precision).abs

    def isEmpty = of.isEmpty

    override def toString = s"|$of|"

}

object RealSeries {

    def apply(left: Real, right: Real): Real = (left, right) match {
        case (Zero, _) => right
        case (_, Zero) => left
        case _ => new RealSeries(Seq(left, right))
    }

    def apply(terms: Seq[Real]): Real = terms.filterNot(_.isEmpty) match {
        case Nil => Zero
        case term :: Nil => term
        case _ => new RealSeries(terms)
    }

}

class RealSeries private(val terms: Seq[Real])
        extends Real
        with ApproximatelyEvaluated {

    private final def series = Series(terms)

    override def toConstant = super[Real].toConstant

    override protected[this] def approx(precision: Precision) = terms.map(_.approximatelyEvaluate(precision)).sum

    override def ?+(that: Real) = {
        if (that.isEmpty) this
        Some(RealSeries(that match {
            case series: RealSeries => simplify(this.terms, series.terms)
            case _ => simplify(that, terms)
        }))
    }

    private def simplify(left: Seq[Real], right: Seq[Real]): Seq[Real] = {
        left.foldLeft(right)((seq, next) => simplify(next, seq))
    }

    private def simplify(term: Real, terms: Seq[Real]): Seq[Real] = {
        var simplified = false
        var current = term
        val series = terms.foldLeft(new mutable.ListBuffer[Real]())((seq, next) => next ?+? current match {
            case Some(m) => {
                simplified = true
                current = m
                seq += current
            }
            case _ => seq :+ next
        })
        if (!simplified) series += current;
        series.toSeq
    }

    def isEmpty = series.isEmpty //TODO not necessarily true!

    override def toString = series.toString

    override def equals(that: Real) = that match {
        case series: RealSeries => this.terms == series.terms || super.equals(series)
        case _ => super.equals(that)
    }

}

object RealProduct {

    def apply(left: Real, right: Real): Real = if (left.isEmpty || right.isEmpty) Zero else new RealProduct(Seq(left, right))

    def apply(terms: Seq[Real]): Real = terms match {
        case Nil => Zero
        case item :: Nil => item
        case _ if terms.contains(Zero) => Zero
        case _ => new RealProduct(terms)
    }

}

class RealProduct private(val terms: Seq[Real])
        extends Real
        with ApproximatelyEvaluated {

    require(!terms.isEmpty)

    private final def product = Product(terms)

    override protected[this] def approx(precision: Precision) = terms.map(_.approximatelyEvaluate(precision)).product

    override def ?*(that: Real) = {
        if (that.isEmpty) Some(Zero)
        else Some(RealProduct(that match {
            case product: RealProduct => simplify(product.terms, this.terms)
            case _ => simplify(that, this.terms)
        }))
    }

    private def simplify(left: Seq[Real], right: Seq[Real]): Seq[Real] = {
        left.foldLeft(right)((seq, next) => simplify(next, seq))
    }

    private def simplify(term: Real, terms: Seq[Real]): Seq[Real] = {
        var simplified = false
        var current = term
        val product = terms.foldLeft(new mutable.ListBuffer[Real]())((seq, next) => next ?*? current match {
            case Some(m) => {
                simplified = true
                current = m
                seq += current
            }
            case _ => seq :+ next
        })
        if (!simplified) current +=: product
        product.toSeq
    }

    def isEmpty = product.isEmpty

    override def toString = product.toString

    override def equals(that: Real) = that match {
        case product: RealProduct => this.terms == product.terms || super.equals(product)
        case _ => super.equals(that)
    }

}