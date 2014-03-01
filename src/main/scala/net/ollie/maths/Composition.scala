package net.ollie.maths

/**
 * A function of a function.
 *
 * Created by Ollie on 05/01/14.
 * @see http://mathworld.wolfram.com/Composition.html
 */
trait Composition
        extends Expression {

    protected[this] def of: Expression

    def variables = of.variables

    def toConstant: Option[Constant] = of.toConstant match {
        case Some(n) => Some(at(n))
        case _ => None
    }

    protected[this] def at(n: Constant): Constant

    def replace(variables: Map[Variable, Expression]) = apply(of.replace(variables))

    /**
     * Chain rule.
     */
    def df(x: Variable) = of.df(x) * derivative(of)

    def unary_-() = Expression.negate(this)

    protected[this] def derivative(x: Expression): Expression

    protected[this] def apply(x: Expression): Expression

}
