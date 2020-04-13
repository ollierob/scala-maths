package net.ollie.maths.functions.angular

import net.ollie.maths.expressions.Expression
import net.ollie.maths.functions.{OddBuiltFunction, RealFunctionBuilder}
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{Precision, Real}

object HyperbolicTan
    extends RealFunctionBuilder {

    def apply(re: Real): Real with HyperbolicTan = new RealHyperbolicTan(re)

    def squared(of: Expression) = HyperbolicTan(HyperbolicTan(of)) //TODO class

    override protected[this] def create(expr: Expression) = new HyperbolicTanOf(expr)

    override protected[this] def empty = Zero

}

trait HyperbolicTan
    extends Expression {

    val of: Expression

    override def toString = s"HyperbolicTan($of)"

}

class HyperbolicTanOf(val of: Expression)
    extends OddBuiltFunction
        with HyperbolicTan {

    protected[this] def builder = HyperbolicTan

    override def isEmpty = of.isEmpty

    protected[this] def derivative(x: Expression) = 1 - HyperbolicTan.squared(of)

}

class RealHyperbolicTan(val of: Real)
    extends Real
        with HyperbolicTan {

    private lazy val value = HyperbolicSin(of) / HyperbolicCos(of)

    override def evaluate(precision: Precision) = value.evaluate(precision)

    override def isEmpty = of.isEmpty
}