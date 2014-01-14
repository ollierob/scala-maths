package net.ollie.maths.numbers.real


import net.ollie.maths.numbers._
import net.ollie.maths.methods.ApproximatelyEvaluated
import scala.math.BigDecimal.RoundingMode

/**
 * Created by Ollie on 12/01/14.
 */
object RealPower {

    def apply(base: RealNumber, power: IntegerNumber)(implicit convention: ZeroToPowerZeroConvention = ZeroToPowerZeroIsOne): RealNumber = (base, power) match {
        case (Zero, Zero) => convention.value
        case (Zero, _) if power > Zero => Zero
        case (Zero, _) if power < Zero => Zero.inverse
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

class RealToIntegerPower(val base: RealNumber, val power: IntegerNumber)
        extends RealNumber
        with ApproximatelyEvaluated {

    def isEmpty = false

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

    override def equals(that: RealNumber) = that match {
        case pow: RealToIntegerPower if base == pow.base => power == pow.power
        case _ => super.equals(that)
    }

    override def toString = s"($base ^ $power)"

}

trait ZeroToPowerZeroConvention {

    def value: RealNumber

}

