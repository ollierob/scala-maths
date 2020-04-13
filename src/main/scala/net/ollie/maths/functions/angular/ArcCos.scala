package net.ollie.maths.functions.angular

import ch.obermuhlner.math.big.BigDecimalMath
import net.ollie.maths.expressions.{Expression, Invertible}
import net.ollie.maths.functions.numeric.SquareRoots
import net.ollie.maths.functions.{BuiltFunction, FunctionBuilder}
import net.ollie.maths.numbers.constants.{One, Pi}
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.{CachedEvaluated, Constant}

/**
 * Created by Ollie on 10/02/14.
 */
/**
 *
 * @see http://mathworld.wolfram.com/InverseCosine.html
 */
object ArcCos
    extends FunctionBuilder {

    import Angle._

    def apply(n: Constant): Angle = n match {
        case re: Real => apply(re)
        case _ => ???
    }

    def apply(re: Real): Angle = new RealArcCos(re) radians

    protected[this] def create(x: Expression) = new ArcCosOf(x)

    protected[this] def empty = Pi / 2

}

trait ArcCos
    extends Expression {

    val of: Expression

    override def toString = s"ArcCos($of)"

}

class ArcCosOf(val of: Expression)
    extends BuiltFunction
        with Invertible
        with ArcCos {

    protected[this] def builder = ArcCos

    protected[this] def derivative(x: Expression) = -1 / SquareRoots(1 - (x ^ 2))

    def inverse = Cos(of)

    def isEmpty = false

}

class RealArcCos(override val of: Real)
    extends Real
        with ArcCos
        with CachedEvaluated {

    protected[this] def doEvaluate(precision: Precision) = BigDecimalMath.acos(of.evaluate(precision).underlying(), precision.toMathContext)

    def isEmpty = of == One

    override def variables = super[Real].variables

    override def toConstant = super[Real].toConstant

    override def inverse = super[Real].inverse

}