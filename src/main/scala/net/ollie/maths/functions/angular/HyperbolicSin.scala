package net.ollie.maths.functions.angular

import net.ollie.maths.functions.{ComplexExpressionBuilder, UnivariateFunction}
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.{BuiltFunction, Expression}
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.functions.numeric.Exp

/**
 * Created by Ollie on 09/02/14.
 */
object HyperbolicSin
        extends ComplexExpressionBuilder
        with UnivariateFunction[Complex, Complex] {

    def apply(re: Real): Real with HyperbolicSin = new RealHyperbolicSin(re)

    def apply(z: Complex): Complex = ???

    protected[this] def empty = Zero

    protected[this] def create(expr: Expression) = new HyperbolicSinExpression(expr)

}

trait HyperbolicSin
        extends Expression {

    def of: Expression

    override def toString = s"Sinh($of)"

}

class HyperbolicSinExpression(val of: Expression)
        extends BuiltFunction
        with HyperbolicSin {

    protected[this] def builder = HyperbolicSin

    def isEmpty = of.isEmpty

    protected[this] def derivative(x: Expression) = HyperbolicCos(x)

}

class RealHyperbolicSin(val of: Real)
        extends Real
        with HyperbolicSin {

    def isEmpty = of.isEmpty

    private lazy val representation = (Exp(of) - Exp(-of)) / 2

    protected[this] def doEvaluate(precision: Precision) = representation.evaluate(precision)

}
