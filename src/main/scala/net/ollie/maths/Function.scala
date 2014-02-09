package net.ollie.maths

import net.ollie.maths.functions.ExpressionBuilder

/**
 * This expression is a function of another expression. (Most are!)
 *
 * Created by Ollie on 05/01/14.
 *
 * @see http://mathworld.wolfram.com/Composition.html
 */
trait Function
        extends Expression {

    protected[this] def of: Expression

    def variables = of.variables

    def toConstant: Option[Number] = of.toConstant match {
        case Some(n) => Some(at(n))
        case _ => None
    }

    protected[this] def at(n: Number): Number

    def replace(variables: Map[Variable, Expression]) = apply(of.replace(variables))

    def df(x: Variable) = of.df(x) * derivative(of)

    def unary_-() = Expression.negate(this)

    protected[this] def derivative(x: Expression): Expression

    protected[this] def apply(x: Expression): Expression

}

trait FunctionBuilder
        extends Function {

    protected[this] def builder: ExpressionBuilder

    protected[this] def at(n: Number) = builder(n)

    protected[this] def apply(x: Expression) = builder(x)

}

/**
 * Builds an odd expression.
 * @see http://mathworld.wolfram.com/OddFunction.html
 */
trait OddFunctionBuilder
        extends FunctionBuilder {

    override def unary_-() = apply(-of)

}
