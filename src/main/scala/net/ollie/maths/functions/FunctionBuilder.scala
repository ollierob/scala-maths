package net.ollie.maths.functions

import net.ollie.maths._
import net.ollie.maths.expressions.{Composition, Expression}
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.complex.Complex

/**
 * Can convert an expression into another expression, or a number into another number.
 *
 * Created by Ollie on 11/01/14.
 *
 * @see [[BuiltFunction]]
 */
trait FunctionBuilder {

    def apply(n: Constant): Constant

    def apply(expression: Expression): Expression = expression.toConstant match {
        case Some(n) => apply(n)
        case _ if expression.isEmpty => empty
        case _ => create(expression)
    }

    protected[this] def create(expr: Expression): Expression

    protected[this] def empty: Expression

}

trait RealFunctionBuilder
    extends FunctionBuilder {

    def apply(n: Constant): Constant = Real(n) match {
        case Some(re) => apply(re)
        case _ => ???
    }

    def apply(re: Real): Constant

}

trait ComplexFunctionBuilder
    extends RealFunctionBuilder {

    type Z <: Constant

    override def apply(n: Constant): Z = Complex(n) match {
        case Some(z) => z.toReal match {
            case Some(re) => apply(re)
            case _ => apply(z)
        }
        case _ => ???
    }

    def apply(re: Real): Z = apply(Complex(re))

    def apply(z: Complex): Z

}

/**
 * Function built by a function builder.
 */
trait BuiltFunction
    extends Composition {

    protected[this] def builder: FunctionBuilder

    override protected[this] def at(n: Constant) = builder(n)

    override protected[this] def apply(x: Expression) = builder(x)

}

/**
 * Builds an odd function.
 *
 * @see http://mathworld.wolfram.com/OddFunction.html
 */
trait OddBuiltFunction
    extends BuiltFunction {

    override def unary_-() = apply(-of)

}