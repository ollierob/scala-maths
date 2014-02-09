package net.ollie.maths.functions.numeric

import scala.Some

import net.ollie.maths.Expression
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 08/01/14.
 */
object SquareRoot {

    private val HALF: Real = One / 2

    def apply(x: Expression): Expression = x ^ HALF

}

object PositiveSquareRoot
        extends UnivariateFunction[PositiveReal, PositiveReal] {

    def apply(f: Real): Option[PositiveReal] = f match {
        case Zero => Some(Zero)
        case _ if f.isStrictlyPositive => Some(apply(f.abs))
        case _ => None
    }

    def apply(f: PositiveReal): PositiveReal = if (f.isEmpty) Zero else new PositiveSquareRoot(f)

    def unapply(root: PositiveSquareRoot): Option[PositiveReal] = Some(root.of)

}

class PositiveSquareRoot(val of: PositiveReal)
        extends AnyRef
        with PositiveReal
        with ApproximatelyEvaluated {

    override def approx(precision: Precision) = {
        if (precision.value < 16) Math.sqrt(of.approximatelyEvaluate(precision).toDouble)
        else ???
    }

    def isEmpty = of.isEmpty

    override def squared = of

    override def ?*(that: Real) = that match {
        case PositiveSquareRoot(fo) => Some(PositiveSquareRoot(of * fo))
        case _ => super.?*(that)
    }

    override def toString = s"+√($of)"

}