package net.ollie.maths.functions.angular

import net.ollie.maths.expressions.{Expression, Invertible}
import net.ollie.maths.functions.numeric.SquareRoots
import net.ollie.maths.functions.{BuiltFunction, RealFunctionBuilder}
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.Zero

trait ArcHyperbolicSin
    extends Expression with Invertible {

}

object ArcHyperbolicSin
    extends RealFunctionBuilder {

    override def apply(re: Real) = ???

    override protected[this] def create(expr: Expression) = new ArcHyperbolicSinOf(expr)

    override protected[this] def empty = Zero

}

class ArcHyperbolicSinOf(val of: Expression)
    extends BuiltFunction with ArcHyperbolicSin {

    override protected[this] def builder = ArcHyperbolicSin

    override protected[this] def derivative(x: Expression) = 1 / SquareRoots(x ^ 2 + 1)

    override def inverse = HyperbolicSin(of)

    override def isEmpty = of.isEmpty

}