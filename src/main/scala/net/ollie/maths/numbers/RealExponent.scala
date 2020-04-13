package net.ollie.maths.numbers

import net.ollie.maths.expressions.Exponentiated
import net.ollie.maths.functions.numeric.NumericRoots
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.{One, Zero}
import net.ollie.maths.{CachedEvaluated, Operation, Variable}
import net.ollie.utils.BigDecimals

/**
 * Created by Ollie on 12/01/14.
 */
trait RealExponent
    extends Multivalued {

    type Contents = Complex

    def base: Real

    def power: Real

    def isEmpty = base.isEmpty

    def values: Set[Complex]

    def principal: Complex = values.head

    def inverse = RealExponent(base, -power)

    override def toString = s"($base ^ $power)"

}

object RealExponent {

    def apply(base: Int, power: Int): Real = apply(Integer(base), Integer(power))

    /**
     * Single-valued integer power.
     */
    def apply(base: Real, power: Integer)(implicit convention: ZeroToPowerZeroConvention = ZeroToPowerZeroIsOne): Real = {
        (base, power) match {
            case (Zero, Zero) => convention.value
            case (Zero, _) if power.isPositive => Zero
            case (Zero, _) => Zero.inverse
            case (One, _) => One
            case (_, Zero) => One
            case (_, One) => base
            case _ => new PrincipalRealToIntegerPower(base, power)
        }
    }

    /**
     * Multi-valued real powers.
     */
    def apply(base: Real, power: Real): RealExponent = power match {
        case r: Rational => new RealToRationalExponent(base, r)
        case _ if base.isPositive && power.isPositive => new PositiveToPositiveExponent(base, power)
        case _ => ???
    }

    trait ZeroToPowerZeroConvention {

        def value: Real

    }

    object ZeroToPowerZeroIsUndefined
        extends ZeroToPowerZeroConvention {

        def value = Operation.undefined

    }

    object ZeroToPowerZeroIsOne
        extends ZeroToPowerZeroConvention {

        def value = One

    }

}

/**
 * x^(a/b) is equal to the bth root of x^a
 *
 * @param base
 * @param power
 */
private class RealToRationalExponent(val base: Real, val power: Rational)
    extends RealExponent {

    private val primary = base ^ power.numerator
    private lazy val roots: NumericRoots[Real, Complex] = NumericRoots(primary, power.denominator)

    override def principal: Complex = roots.principal

    def values: Set[Complex] = roots.values

}

class PrincipalRealToIntegerPower(val base: Real, val power: Integer)
    extends Real
        with Exponentiated
        with CachedEvaluated {

    override def ?*(that: Real) = that match {
        case pow: PrincipalRealToIntegerPower if base == pow.base => Some(RealExponent(base, power + pow.power))
        case _ if that == base => Some(RealExponent(base, power + 1))
        case _ if that == -base => Some(-RealExponent(base, power + 1))
        case _ => super.?*(that)
    }

    private lazy val negate: Boolean = !base.isPositive && !power.isEven

    private val baseAbs = base.abs

    override def ?==(that: Real) = that match {
        case pow: PrincipalRealToIntegerPower if base == pow.base => Some(power == pow.power)
        case _ => super.?==(that)
    }

    override protected[this] def doEvaluate(precision: Precision): BigDecimal = {
        val bd: BigDecimal = BigDecimals.pow(baseAbs.evaluate(precision), power.evaluate(precision), precision)
        if (negate) -bd else bd
    }

    override def df(x: Variable) = super[Real].df(x)

}

private class PositiveToPositiveExponent(val base: Real, val power: Real)
    extends RealExponent {

    override def values: Set[Complex] = ???

}