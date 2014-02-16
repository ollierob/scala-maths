package net.ollie.maths.numbers

import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.constants.{Zero, One}
import org.nevec.rjm.BigDecimalMath

/**
 * Created by Ollie on 12/01/14.
 */
trait RealPower
        extends Real {

    protected def base: Real

    protected def power: Real

    override def isEmpty = base.isEmpty

    override def toString = s"($base ^ $power)"

}

object RealPower {

    def apply(base: Real, power: Integer)(implicit convention: ZeroToPowerZeroConvention = ZeroToPowerZeroIsOne): Real = {
        (base, power) match {
            case (Zero, Zero) => convention.value
            case (Zero, _) if power.isStrictlyPositive => Zero
            case (Zero, _) => Zero.inverse
            case (One, _) => One
            case (_, Zero) => One
            case (_, One) => base
            case _ => new RealToIntegerPower(base, power)
        }
    }

    object ZeroToPowerZeroIsUndefined
            extends ZeroToPowerZeroConvention {

        def value = ???

    }

    object ZeroToPowerZeroIsOne
            extends ZeroToPowerZeroConvention {

        def value = One

    }

}

class RealToIntegerPower(val base: Real, val power: Integer)
        extends RealPower
        with ApproximatelyEvaluated {

    override def ?*(that: Real) = that match {
        case pow: RealToIntegerPower if base == pow.base => Some(RealPower(base, power + pow.power))
        case _ if that == base => Some(RealPower(base, power + 1))
        case _ if that == -base => Some(-RealPower(base, power + 1))
        case _ => super.?*(that)
    }

    override def approx(precision: Precision) = {
        BigDecimalMath.pow(base.evaluate(precision).underlying(), power.evaluate(precision).underlying())
    }

    override def ?==(that: Real) = that match {
        case pow: RealToIntegerPower if base == pow.base => Some(power == pow.power)
        case _ => super.?==(that)
    }

}

trait ZeroToPowerZeroConvention {

    def value: Real

}

