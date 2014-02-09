package net.ollie.maths

/**
 * Some function of an expression.
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
