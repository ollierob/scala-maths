package net.ollie.maths.functions.numeric

import scala.math.BigDecimal.RoundingMode

import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers.{PositiveRealNumber, Precision, RealNumber}

/**
 * Created by Ollie on 08/01/14.
 */
object PositiveSquareRoot
        extends UnivariateFunction[PositiveRealNumber, PositiveRealNumber] {

    def apply(f: PositiveRealNumber): PositiveRealNumber = if (f.isEmpty) f else new PositiveSquareRoot(f)

    def unapply(root: PositiveSquareRoot): Option[PositiveRealNumber] = Some(root.of)

}

class PositiveSquareRoot(val of: PositiveRealNumber)
        extends AnyRef
        with PositiveRealNumber {

    protected[this] def eval(precision: Precision)(implicit mode: RoundingMode.RoundingMode): BigDecimal = ???

    def isEmpty = of.isEmpty

    override def ?*(that: RealNumber) = that match {
        case PositiveSquareRoot(fo) => Some(PositiveSquareRoot(of * fo))
        case _ => super.?*(that)
    }

    override def toString = "+√(" + of + ")"

}