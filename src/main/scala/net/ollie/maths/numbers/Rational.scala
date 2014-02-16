package net.ollie.maths.numbers

import net.ollie.maths.ExpressionFraction
import net.ollie.maths.functions.numeric.GreatestCommonDivisor
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.constants.{MinusOne, Zero, One}

/**
 * A number that can be expressed as an integer divided by another integer.
 * Created by Ollie on 04/01/14.
 * @see http://mathworld.wolfram.com/RationalNumber.html
 */
trait Rational
        extends Real {

    def numerator: Integer

    def denominator: Integer

    override def abs: PositiveReal with Rational = numerator.abs / denominator.abs

    override def inverse: Rational = denominator / numerator

    override def unary_-(): Rational = (-numerator) / denominator

    override def squared: Rational with PositiveReal = numerator.squared / denominator.squared

    def isEmpty: Boolean = numerator.isEmpty

    override def approximatelyEvaluate(precision: Precision): BigDecimal = {
        precision(numerator.approximatelyEvaluate(precision) / denominator.approximatelyEvaluate(precision))
    }

    override def ?+(that: Real) = that match {
        case r: Rational => Some(this + r)
        case _ => super.?+(that)
    }

    override def ?*(that: Real) = that match {
        case r: Rational => Some(IntegerFraction(r.numerator * numerator, r.denominator * denominator))
        case _ if that == denominator => Some(numerator)
        case _ => super.?*(that)
    }

    override def ?==(that: Real) = that match {
        case r: Rational => Some(this.numerator == r.numerator && this.denominator == r.denominator)
        case _ => super.?==(that)
    }

    def +(that: Rational): Rational = {
        val n: Integer = (this.numerator * that.denominator) + (that.numerator * this.denominator)
        val d: Integer = this.denominator * that.denominator
        n / d
    }

    def -(that: Rational): Rational = this + (-that)

    def *(that: Rational): Rational = (this.numerator * that.numerator) / (this.denominator * that.denominator)

    def /(that: Rational): Rational = (this.numerator * that.denominator) / (this.denominator * that.numerator)

    override def toString: String = numerator.toString + "/" + denominator.toString

}

object Rational {

    implicit object Numeric
            extends scala.Numeric[Rational] {

        def compare(x: Rational, y: Rational) = x.compareTo(y)

        def toDouble(x: Rational) = x.evaluate(DoublePrecision).toDouble

        def toFloat(x: Rational) = x.evaluate(SinglePrecision).toFloat

        def toLong(x: Rational) = x.evaluate(IntegerPrecision).toLong

        def toInt(x: Rational) = x.evaluate(IntegerPrecision).toInt

        def fromInt(x: Int) = Integer(x)

        def negate(x: Rational) = -x

        def times(x: Rational, y: Rational) = x * y

        def minus(x: Rational, y: Rational) = x - y

        def plus(x: Rational, y: Rational) = x + y

    }

}

object IntegerFraction {

    def apply(numerator: Integer, denominator: Integer): Rational = common(numerator, denominator) match {
        case Some(m) => m
        case _ => (numerator, denominator) match {
            case (n1: Natural, n2: Natural) => Natural.divide(n1, n2)
            case _ => reduce(numerator, denominator) match {
                case Some((n, d)) => apply(n, d)
                case _ => new IntegerFraction(numerator, denominator)
            }
        }
    }

    def common(numerator: Integer, denominator: Integer): Option[Rational] = (numerator, denominator) match {
        case (_, Zero) => ??? //Some(numerator * denominator.inverse)
        case (Zero, _) => Some(Zero)
        case (_, One) => Some(numerator)
        case (_, MinusOne) => Some(-numerator)
        case _ if numerator == denominator => Some(One)
        case _ => None
    }

    def reduce(numerator: Integer, denominator: Integer): Option[(Integer, Integer)] = {
        GreatestCommonDivisor(numerator, denominator) match {
            case One => None
            case gcd => Some(Integer(numerator.evaluate / gcd.evaluate), Integer(denominator.evaluate / gcd.evaluate))
        }
    }

}

class IntegerFraction private[numbers](override val numerator: Integer, override val denominator: Integer)
        extends ExpressionFraction(numerator, denominator)
        with Rational
        with ApproximatelyEvaluated {

    require(!denominator.isEmpty)

    override def ?*(that: Real) = numerator ?* that match {
        case Some(m: Integer) => Some(IntegerFraction(m, denominator))
        case _ => super.?*(that)
    }

    override def /(that: Real) = that match {
        case rational: Rational => this * rational.inverse
        case _ => super./(that)
    }

    override def isEmpty = super[ExpressionFraction].isEmpty

    override def toConstant = super[Rational].toConstant

    override def variables = super[Rational].variables

    override def approximatelyEvaluate(precision: Precision) = super[Rational].approximatelyEvaluate(precision)

    protected[this] def approx(precision: Precision) = approximatelyEvaluate(precision)

}
