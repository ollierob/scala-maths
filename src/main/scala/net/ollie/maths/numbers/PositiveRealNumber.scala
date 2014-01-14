package net.ollie.maths.numbers

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

    override def isStrictlyPositive = !this.isEmpty

}

object PositiveRealNumber {

    def inverse(re: PositiveRealNumber): PositiveRealNumber = new PositiveRealInverseNumber(re)

}

class PositiveRealInverseNumber(val re: PositiveRealNumber)
        extends InverseRealNumber(re)
        with PositiveRealNumber
