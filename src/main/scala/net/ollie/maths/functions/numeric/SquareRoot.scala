package net.ollie.maths.functions.numeric

import scala.math.BigDecimal.RoundingMode

import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers.{PositiveRealNumber, Precision, RealNumber}
import net.ollie.maths.methods.ApproximatelyEvaluated

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
        with PositiveRealNumber
        with ApproximatelyEvaluated {

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode.RoundingMode) = {
        if (precision.value < 16) Math.sqrt(of.approximatelyEvaluate(precision).toDouble)
        else ???
    }

    def isEmpty = of.isEmpty

    override def ?*(that: RealNumber) = that match {
        case PositiveSquareRoot(fo) => Some(PositiveSquareRoot(of * fo))
        case _ => super.?*(that)
    }

    override def toString = "+√(" + of + ")"

}