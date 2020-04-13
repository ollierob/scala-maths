package net.ollie.maths.functions.numeric

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.{Half, One, Two, Zero}
import net.ollie.maths.{CachedEvaluated, Constant}
import net.ollie.utils.BigDecimals

/**
 * Created by Ollie on 08/01/14.
 */
trait SquareRoots[C <: Constant, T <: Constant]
    extends NumericRoots[C, T] {

    override def degree = 2

    override def toString = s"SquareRoots($of)"

}

object SquareRoots {

    def apply(x: Expression): Expression = x ^ Half

    def apply(re: Real): NumericRoots[Real, Complex] = NumericRoots(re, 2)

    def apply(re: PositiveReal) = new RealSquareRoots(re)

    def apply(z: Complex): NumericRoots[_, Complex] = {
        if (z.isReal) apply(z.re)
        else new ComplexSquareRoots(z)
    }

}

class RealSquareRoots(val of: PositiveReal)
    extends SquareRoots[PositiveReal, Real] {

    override def inverse = NumericRoots(of.inverse, degree)

    private lazy val root: PositiveReal = PositiveSquareRoot(of)

    def principal = root

    def values = Set(root, -root)

}

object PositiveSquareRoot
    extends UnivariateFunction[PositiveReal, PositiveReal] {

    def apply(i: Int): PositiveReal = i match {
        case _ if i < 0 => ???
        case 0 => Zero
        case 1 => One
        case 2 => PositiveSquareRootTwo
        case _ => new PositiveSquareRoot(i)
    }

    def apply(re: Real): Option[PositiveReal] = re match {
        case Zero => Some(Zero)
        case _ if re.isPositive => Some(apply(re.abs))
        case _ => None
    }

    def apply(r: PositiveReal): PositiveReal = r match {
        case Zero => Zero
        case One => One
        case _ => new PositiveSquareRoot(r)
    }

    def unapply(root: PositiveSquareRoot): Option[PositiveReal] = Some(root.of)

}

class PositiveSquareRoot(val of: PositiveReal)
    extends PositiveReal with CachedEvaluated {

    override def doEvaluate(precision: Precision) = {
        BigDecimals.sqrt(of.evaluate(precision), precision)
    }

    def isEmpty = of.isEmpty

    override def ^(that: Integer) = that match {
        case Two => squared
        case _ => super.^(that)
    }

    override def squared = of

    override def ?*(that: Real) = that match {
        case PositiveSquareRoot(fo) => Some(PositiveSquareRoot(of * fo))
        case _ => super.?*(that)
    }

    override def toString = s"+√($of)" //Symbol usually means the positive root

}

object PositiveSquareRootTwo
    extends PositiveSquareRoot(2)

class ComplexSquareRoots(override val of: Complex)
    extends SquareRoots[Complex, Complex] {

    lazy val values = ComplexNthRoots(of, 2)

    override def principal = values.head

}