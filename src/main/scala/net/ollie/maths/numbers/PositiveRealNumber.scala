package net.ollie.maths.numbers


import net.ollie.maths.numbers.real.RealPower
import org.nevec.rjm.BigDecimalMath

/**
 * Numbers known to be equal to or greater than zero at compile time.
 * Created by Ollie on 04/01/14.
 */
trait PositiveRealNumber
        extends RealNumber {

    override def abs = this

    override def inverse: PositiveRealNumber = PositiveRealNumber.inverse(this)

    def +(that: PositiveRealNumber): PositiveRealNumber = super.+(that).abs

    def *(that: PositiveRealNumber): PositiveRealNumber = super.*(that).abs

    def /(that: PositiveRealNumber): PositiveRealNumber = this * that.inverse

    def ^(that: RealNumber): PositiveRealNumber = PositiveRealNumber.pow(this, that)

    override def isStrictlyPositive = !this.isEmpty

}

object PositiveRealNumber {

    implicit def apply(int: Int): NaturalNumber = NaturalNumber(int)

    def inverse(re: PositiveRealNumber): PositiveRealNumber = new PositiveRealInverseNumber(re)

    def pow(base: PositiveRealNumber, power: RealNumber): PositiveRealNumber = new PositiveRealPower(base, power)

}

class PositiveRealInverseNumber(val re: PositiveRealNumber)
        extends InverseRealNumber(re)
        with PositiveRealNumber

/**
 *
 * @param base
 * @param power
 * @see http://arxiv.org/abs/0908.3030v2
 */
class PositiveRealPower(val base: PositiveRealNumber, val power: RealNumber)
        extends PositiveRealNumber
        with RealPower {

    protected[this] def eval(precision: Precision) = {
        BigDecimalMath.pow(base.evaluate(precision).underlying(), power.evaluate(precision).underlying())
    }

}