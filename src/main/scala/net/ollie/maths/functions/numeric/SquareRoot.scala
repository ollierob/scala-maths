package net.ollie.maths.functions.numeric

import scala.Some

import net.ollie.maths.{CachedEvaluated, Expression}
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{One, Half, Zero}
import org.nevec.rjm.BigDecimalMath

/**
 * Created by Ollie on 08/01/14.
 */
object SquareRoot {

    def apply(x: Expression): Expression = x ^ Half

    def apply(re: PositiveReal) = new RealSquareRoots(re)

}

class RealSquareRoots(val of: PositiveReal)
        extends Roots[PositiveReal, Real] {

    final def degree = 2

    def inverse = Roots(of.inverse, degree)

    private lazy val root: PositiveReal = PositiveSquareRoot(of)

    def principal = root

    def values = Set(root, -root)

    override def toString = s"±SquareRoot($of)"

}

object PositiveSquareRoot
        extends UnivariateFunction[PositiveReal, PositiveReal] {

    def apply(i: Int): PositiveReal = i match {
        case 0 => Zero
        case 1 => One
        case 2 => PositiveSquareRootTwo
        case _ => PositiveSquareRoot(PositiveReal(i))
    }

    def apply(re: Real): Option[PositiveReal] = re match {
        case Zero => Some(Zero)
        case _ if re.isStrictlyPositive => Some(apply(re.abs))
        case _ => None
    }

    def apply(f: PositiveReal): PositiveReal = {
        if (f.isEmpty) Zero
        else new PositiveSquareRoot(f)
    }

    def unapply(root: PositiveSquareRoot): Option[PositiveReal] = Some(root.of)

}

class PositiveSquareRoot(val of: PositiveReal)
        extends AnyRef
        with PositiveReal
        with CachedEvaluated {

    override def doEvaluate(precision: Precision) = {
        if (precision.digits < 16) Math.sqrt(of.approximatelyEvaluate(precision).toDouble)
        else BigDecimalMath.sqrt(of.evaluate(precision).underlying())
    }

    def isEmpty = of.isEmpty

    override def squared = of

    override def ?*(that: Real) = that match {
        case PositiveSquareRoot(fo) => Some(PositiveSquareRoot(of * fo))
        case _ => super.?*(that)
    }

    override def toString = s"+√($of)"

}

object PositiveSquareRootTwo
        extends PositiveSquareRoot(2)

