package net.ollie.maths

import net.ollie.maths.methods.{Product, Series}
import net.ollie.maths.numbers.{IntegerNumber, Zero}

/**
 * Created by Ollie on 01/01/14.
 */
trait Expression
        extends Differentiable {

    def unary_-(): Expression = Expression.negate(this)

    def replace(variable: Variable, expression: Option[Expression]): Expression = expression match {
        case Some(expr) => replace(variable, expr)
        case _ => this
    }

    def replace(variable: Variable, expression: Expression): Expression = if (variable == expression) this else replace(Map((variable, expression)))

    def replace(variables: Map[Variable, Expression]): Expression

    def toConstant: Option[Number]

    def variables: Set[Variable]

    def isEmpty: Boolean

    def +(that: Expression): Expression = Expression.series(this, that)

    def -(that: Expression): Expression = this + (-that)

    def *(that: Expression): Expression = Expression.product(this, that)

    def /(that: Expression): Expression = Expression.divide(this, that)

    def ^(that: Expression): Expression = ??? //TODO

    override def df(x: Variable): Expression

    final override def equals(obj: Any): Boolean = obj match {
        case expr: Expression => this.equals(expr)
        case _ => super.equals(obj)
    }

    def equals(expr: Expression): Boolean = if (this.isEmpty) expr.isEmpty else super.equals(expr)

}

object Expression {

    def negate(expression: Expression) = new NegatedExpression(expression)

    def divide(numerator: Expression, denominator: Expression): Expression = (numerator, denominator) match {
        case _ if numerator.isEmpty => Zero
        case _ => new ExpressionFraction(numerator, denominator)
    }

    def series(e1: Expression, e2: Expression) = Series(e1, e2)

    def product(e1: Expression, e2: Expression) = Product(e1, e2)

    implicit def convert(int: Int): IntegerNumber = IntegerNumber(int)

}

class NegatedExpression(val of: Expression)
        extends Expression {

    def replace(variables: Map[Variable, Expression]) = -(of.replace(variables))

    def isEmpty = of.isEmpty

    override def toConstant: Option[Number] = of.toConstant match {
        case Some(n) => Some(-n)
        case _ => None
    }

    def variables = of.variables

    override def toString = "-(" + of + ")"

    def df(x: Variable) = -(of.df(x))
}

class ExpressionFraction(val numerator: Expression, val denominator: Expression)
        extends Expression {

    override def unary_-() = (-numerator) / denominator

    def replace(variables: Map[Variable, Expression]) = numerator.replace(variables) / denominator.replace(variables)

    def toConstant = numerator.toConstant match {
        case Some(n: Number) => denominator.toConstant match {
            case Some(d: Number) => n ?* d.inverse
            case _ => None
        }
        case _ => None
    }

    def variables = numerator.variables ++: denominator.variables

    def isEmpty = numerator.isEmpty

    override def df(x: Variable) = (numerator.df(x) / denominator) - (denominator.df(x) * numerator / denominator)

    override def toString = s"($numerator/$denominator)"

}

trait Nonvariate
        extends Expression {

    def variables = Set()

}

object Univariate {

    implicit def convert(expression: Expression): Univariate = expression match {
        case u: Univariate => u
        case _ => new UnivariateWrapper(expression)
    }

    private class UnivariateWrapper(expression: Expression)
            extends Univariate {

        require(expression.variables.size == 1)

        final val x = expression.variables.iterator.next()

        def replace(variables: Map[Variable, Expression]) = expression.replace(variables)

        def toConstant = expression.toConstant

        def isEmpty = expression.isEmpty

        def variable = x

        override def df(x: Variable) = expression.df(x)

    }

}

trait Univariate
        extends Expression {

    def variable: Variable

    def variables = Set(variable)

    def apply[N <: Number](n: N)(implicit conversion: IdentityArithmetic[Number, N#System]): N#System = conversion.convert(replace(variable, n).toConstant).get

    def dx: Univariate = df(variable)

}
