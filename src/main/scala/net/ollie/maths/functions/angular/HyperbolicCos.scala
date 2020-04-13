package net.ollie.maths.functions.angular

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.numeric.Exp
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder, UnivariateFunction}
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.One
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.{CachedEvaluated, Constant}

/**
 * Created by Ollie on 09/02/14.
 */
object HyperbolicCos
    extends RealFunctionBuilder
        with UnivariateFunction[Complex, Complex] {

    override def apply(n: Constant): Constant = Complex(n) match {
        case Some(z) => apply(z)
        case _ => super.apply(n)
    }

    def apply(re: Real): Real with HyperbolicCos = new RealHyperbolicCos(re)

    def apply(z: Complex): Complex with HyperbolicCos = ???

    override protected[this] def empty = One

    protected[this] def create(expr: Expression) = new HyperbolicCosOf(expr)

}

trait HyperbolicCos
    extends Expression {

    def of: Expression

    override def toString = s"HyperbolicCos($of)"

}

class HyperbolicCosOf(val of: Expression)
    extends BuiltFunction
        with HyperbolicCos {

    override def isEmpty = false

    protected[this] def derivative(x: Expression) = HyperbolicSin(x)

    protected[this] def builder = HyperbolicCos

}

class RealHyperbolicCos(val of: Real)
    extends Real
        with HyperbolicCos
        with CachedEvaluated {

    override def isEmpty = false

    private lazy val representation = (Exp(of) + Exp(-of)) / 2

    protected[this] def doEvaluate(precision: Precision) = representation.evaluate(precision)

}