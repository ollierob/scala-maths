package net.ollie.maths.numbers

import net.ollie.maths.numbers.real.RealPower
import org.nevec.rjm.BigDecimalMath

/**
 * Numbers known to be equal to or greater than zero at compile time.
 * Created by Ollie on 04/01/14.
 */
trait PositiveReal
        extends Real {

    override def abs = this

    override def inverse: PositiveReal = PositiveReal.inverse(this)

    def +(that: PositiveReal): PositiveReal = super.+(that).abs

    def *(that: PositiveReal): PositiveReal = super.*(that).abs

    def /(that: PositiveReal): PositiveReal = this * that.inverse

    def ^(that: Real): PositiveReal = PositiveReal.pow(this, that)

    override def isStrictlyPositive = !this.isEmpty

}

object PositiveReal {

    implicit def apply(int: Int): Natural = Natural(int)

    def inverse(re: PositiveReal): PositiveReal = new PositiveRealInverse(re)

    def pow(base: PositiveReal, power: Real): PositiveReal = new PositiveRealPower(base, power)

}

class PositiveRealInverse(val re: PositiveReal)
        extends InverseReal(re)
        with PositiveReal

/**
 *
 * @param base
 * @param power
 * @see http://arxiv.org/abs/0908.3030v2
 */
class PositiveRealPower(val base: PositiveReal, val power: Real)
        extends PositiveReal
        with RealPower {

    protected[this] def doEvaluate(precision: Precision) = {
        BigDecimalMath.pow(base.evaluate(precision).underlying(), power.evaluate(precision).underlying())
    }

}