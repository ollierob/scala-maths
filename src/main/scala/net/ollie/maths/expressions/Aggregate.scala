package net.ollie.maths.expressions

import net.ollie.maths.Variable

/**
 * Created by Ollie on 11/01/14.
 */
trait Aggregate
        extends Expression {

    protected def terms: Seq[Expression]

    def unary_-() = Expression.negate(this)

    def variables = terms flatMap (_.variables) toSet

    def replace(variables: Map[Variable, Expression]) = apply(terms.map(_.replace(variables)))

    protected[this] def apply(expressions: Seq[Expression]): Expression

    override def equals(that: Expression) = that match {
        case a: Aggregate => this.terms == a.terms //Note that ordering matters here
        case _ => super.equals(that)
    }

}