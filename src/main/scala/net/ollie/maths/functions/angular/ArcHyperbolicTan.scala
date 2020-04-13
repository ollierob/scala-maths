package net.ollie.maths.functions.angular

import net.ollie.maths.expressions.{Expression, Invertible}
import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder}
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.numbers.{Precision, Real}

trait ArcHyperbolicTan
    extends Expression with Invertible {

    val of: Expression

    override def toString = s"ArcHyperbolicTan($of)"

}

object ArcHyperbolicTan
    extends RealFunctionBuilder {

    override def apply(re: Real) = new RealArcHypberbolicTan(re)

    override protected[this] def create(expr: Expression) = new ArcHyperbolicTanOf(expr)

    override protected[this] def empty = Zero

}

class ArcHyperbolicTanOf(val of: Expression)
    extends BuiltFunction with ArcHyperbolicTan {

    override protected[this] def builder = ArcHyperbolicTan

    override protected[this] def derivative(x: Expression) = 1 / (1 - x ^ 2)

    override def inverse = ArcHyperbolicTan(of)

    override def isEmpty = of.isEmpty

}

class RealArcHypberbolicTan(val of: Real)
    extends Real with ArcHyperbolicTan {

    require(of.abs < 1, "Require |arg| < 1")

    private lazy val value: Real = (Ln((1 + of) / (1 - of)) / 2).re

    override def evaluate(precision: Precision) = value.evaluate(precision)

    override def isEmpty = of.isEmpty

}