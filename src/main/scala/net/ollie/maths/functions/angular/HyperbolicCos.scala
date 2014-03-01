package net.ollie.maths.functions.angular

import net.ollie.maths.functions.{RealFunctionBuilder, BuiltFunction, UnivariateFunction}
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.{CachedEvaluated, Number, Expression}
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.numbers.constants.One

/**
 * Created by Ollie on 09/02/14.
 */
object HyperbolicCos
        extends RealFunctionBuilder
        with UnivariateFunction[Complex, Complex] {

    override def apply(n: Number): Number = Complex(n) match {
        case Some(z) => apply(z)
        case _ => super.apply(n)
    }

    def apply(re: Real): Real with HyperbolicCos = new RealHyperbolicCos(re)

    def apply(z: Complex): Complex with HyperbolicCos = ???

    protected[this] def empty = One

    protected[this] def create(expr: Expression) = new HyperbolicCosOf(expr)

}

trait HyperbolicCos {

    def of: Expression

    override def toString = s"HyperbolicCos($of)"

}

class HyperbolicCosOf(val of: Expression)
        extends BuiltFunction
        with HyperbolicCos {

    def isEmpty = false

    protected[this] def derivative(x: Expression) = HyperbolicSin(x)

    protected[this] def builder = HyperbolicCos

}

class RealHyperbolicCos(val of: Real)
        extends Real
        with HyperbolicCos
        with CachedEvaluated {

    def isEmpty = false

    private lazy val representation = (Exp(of) + Exp(-of)) / 2

    protected[this] def doEvaluate(precision: Precision) = representation.evaluate(precision)

}