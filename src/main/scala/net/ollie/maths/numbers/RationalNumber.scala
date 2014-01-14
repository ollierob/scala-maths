package net.ollie.maths.numbers

import scala.math.BigDecimal.RoundingMode.RoundingMode

import net.ollie.maths.functions.numeric.GreatestCommonDivisor
import net.ollie.maths.methods.ApproximatelyEvaluated

/**
 * A number that can be expressed as an integer divided by another integer.
 * Created by Ollie on 04/01/14.
 * @see http://mathworld.wolfram.com/RationalNumber.html
 */
trait RationalNumber
        extends RealNumber {

    def numerator: IntegerNumber

    def denominator: IntegerNumber

    override def inverse = IntegerFraction(denominator, numerator)

    def isEmpty: Boolean = numerator.isEmpty

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode): BigDecimal = numerator.evaluate(precision) / denominator.evaluate(precision)

    override def ?+(that: RealNumber) = that match {
        case r: RationalNumber => Some(IntegerFraction((numerator * r.numerator) + (r.numerator * denominator), denominator * r.denominator))
        case _ => super.?+(that)
    }

    override def ?*(that: RealNumber) = that match {
        case r: RationalNumber => Some(IntegerFraction(r.numerator * numerator, r.denominator * denominator))
        case d: RealNumber if d == denominator => Some(numerator)
        case _ => super.?*(that)
    }

    override def ?==(that: RealNumber) = that match {
        case r: RationalNumber => Some(this.numerator == r.numerator && this.denominator == r.denominator)
        case _ => super.?==(that)
    }

    override def toString: String = numerator.toString + "/" + denominator.toString

}

object IntegerFraction {

    def apply(numerator: IntegerNumber, denominator: IntegerNumber)(implicit doReduce: Boolean = true): RealNumber = (numerator, denominator) match {
        case (_, Zero) => numerator * denominator.inverse
        case (Zero, _) => Zero
        case (_, One) => numerator
        case _ if numerator == denominator => One
        case _ if doReduce => reduce(numerator, denominator) match {
            case Some((n, d)) => apply(n, d)(false)
            case _ => new IntegerFraction(numerator, denominator)
        }
        case _ => new IntegerFraction(numerator, denominator)
    }

    private def reduce(numerator: IntegerNumber, denominator: IntegerNumber): Option[(IntegerNumber, IntegerNumber)] = {
        GreatestCommonDivisor(numerator, denominator) match {
            case One => None
            case gcd => Some(IntegerNumber(numerator.evaluate / gcd.evaluate), IntegerNumber(denominator.evaluate / gcd.evaluate))
        }
    }

}

final class IntegerFraction(val numerator: IntegerNumber, val denominator: IntegerNumber)
        extends RationalNumber
        with ApproximatelyEvaluated {

    require(!denominator.isEmpty)

    override def /(that: RealNumber) = that match {
        case rational: RationalNumber => this * rational.inverse
        case _ => super./(that)
    }

    override def toString = s"IntegerFraction($numerator/$denominator)"

}