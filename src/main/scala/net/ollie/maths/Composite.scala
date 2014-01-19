package net.ollie.maths

/**
 * This expression is composed with another expression. (Most are!)
 * Created by Ollie on 05/01/14.
 * @see http://mathworld.wolfram.com/Composition.html
 */
trait Composite
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

    protected[this] def derivative(at: Expression): Expression

    protected[this] def apply(at: Expression): Expression

}
