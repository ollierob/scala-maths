package net.ollie.maths

/**
 *
 * Created by Ollie on 05/01/14.
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

    protected[this] def apply(expr: Expression): Expression

}
