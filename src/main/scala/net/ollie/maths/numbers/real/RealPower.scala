package net.ollie.maths.numbers.real


import scala.math.BigDecimal.RoundingMode

import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 12/01/14.
 */
trait RealPower
        extends RealNumber {

    protected def base: RealNumber

    protected def power: RealNumber

    override def isEmpty = base.isEmpty

    override def toString = s"($base ^ $power)"

}

object RealPower {

    def apply(base: RealNumber, power: IntegerNumber)(implicit convention: ZeroToPowerZeroConvention = ZeroToPowerZeroIsOne): RealNumber = (base, power) match {
        case (Zero, Zero) => convention.value
        case (Zero, _) if power > Zero => Zero
        case (Zero, _) if power < Zero => Zero.inverse
        case (One, _) => One
        case (_, Zero) => One
        case (_, One) => base
        case _ => new RealToIntegerPower(base, power)
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

private class RealToIntegerPower(val base: RealNumber, val power: IntegerNumber)
        extends RealPower
        with ApproximatelyEvaluated {

    override def ?*(that: RealNumber) = that match {
        case pow: RealToIntegerPower if base == pow.base => Some(RealPower(base, power + pow.power))
        case _ if that == base => Some(RealPower(base, power + 1))
        case _ if that == -base => Some(-RealPower(base, power + 1))
        case _ => super.?*(that)
    }

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode.RoundingMode) = {
        if (precision.value < 16) Math.pow(base.evaluate(precision).toDouble, power.evaluate(precision).toDouble)
        else throw new UnsupportedOperationException(s"Precision $precision unsupported for evaluation of $this")
    }

    override def ?==(that: RealNumber) = that match {
        case pow: RealToIntegerPower if base == pow.base => Some(power == pow.power)
        case _ => super.?==(that)
    }

}

trait ZeroToPowerZeroConvention {

    def value: RealNumber

}

