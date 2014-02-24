package net.ollie.maths.functions.angular

import net.ollie.maths.functions.{RealFunctionBuilder, BuiltFunction, UnivariateFunction}
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.{CachedEvaluated, Number, Expression}
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.functions.numeric.Exp

/**
 * Created by Ollie on 09/02/14.
 */
object HyperbolicSin
        extends RealFunctionBuilder
        with UnivariateFunction[Complex, Complex] {

    override def apply(n: Number): Number = Complex(n) match {
        case Some(z) => apply(z)
        case _ => super.apply(n)
    }

    def apply(re: Real): Real with HyperbolicSin = new RealHyperbolicSin(re)

    def apply(z: Complex): Complex with HyperbolicSin = ???

    protected[this] def empty = Zero

    protected[this] def create(expr: Expression) = new HyperbolicSinOf(expr)

}

trait HyperbolicSin
        extends Expression {

    def of: Expression

    override def toString = s"Sinh($of)"

}

class HyperbolicSinOf(val of: Expression)
        extends BuiltFunction
        with HyperbolicSin {

    protected[this] def builder = HyperbolicSin

    def isEmpty = of.isEmpty

    protected[this] def derivative(x: Expression) = HyperbolicCos(x)

}

class RealHyperbolicSin(val of: Real)
        extends Real
        with HyperbolicSin
        with CachedEvaluated {

    def isEmpty = of.isEmpty

    private lazy val representation = (Exp(of) - Exp(-of)) / 2

    protected[this] def doEvaluate(precision: Precision) = representation.evaluate(precision)

}
