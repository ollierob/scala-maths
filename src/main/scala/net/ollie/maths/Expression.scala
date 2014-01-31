package net.ollie.maths

import net.ollie.maths.functions.numeric.Ln
import net.ollie.maths.methods.{Product, Series}
import net.ollie.maths.numbers.{Integer, Zero}

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

    /**
     * This times that. Will create an expression product if the multiplication cannot be simplified.
     * @param that
     * @return
     */
    final def *(that: Expression): Expression = this ?*? that match {
        case Some(x) => x
        case _ => Expression.product(this, that)
    }

    /**
     * This try-times-try that.
     * Attempts to simplify to a single term, where possible, by multiplying this by the right term,
     * then the right term multiplied by this on the left.
     * @param that
     * @return
     */
    final def ?*?(that: Expression): Option[Expression] = this.?*(that)(true) match {
        case Some(x) => Some(x)
        case _ => that.?*(this)(false) match {
            case Some(x) => Some(x)
            case _ => None
        }
    }

    /**
     * This try-times that.
     * @param that
     * @return
     */
    def ?*(that: Expression)(leftToRight: Boolean): Option[Expression] = None

    def /(that: Expression): Expression = Expression.divide(this, that)

    def ^(that: Expression): Expression = Expression.power(this, that)

    override def df(x: Variable): Expression

    final override def equals(obj: Any): Boolean = obj match {
        case expr: Expression => this.equals(expr)
        case _ => super.equals(obj)
    }

    def equals(expr: Expression): Boolean = if (this.isEmpty) expr.isEmpty else super.equals(expr)

}

object Expression {

    def negate(expr: Expression) = new NegatedExpression(expr)

    def divide(numerator: Expression, denominator: Expression): Expression = (numerator, denominator) match {
        case _ if numerator.isEmpty => Zero
        case _ => new ExpressionFraction(numerator, denominator)
    }

    def power(base: Expression, power: Expression) = (base, power) match {
        case (Zero, _) => Zero
        case _ => new ExpressionPower(base, power)
    }

    def series(e1: Expression, e2: Expression) = Series(e1, e2)

    def product(e1: Expression, e2: Expression) = Product(e1, e2)

    implicit def convert(int: Int): Integer = Integer(int)

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

    def toConstant: Option[Number] = numerator.toConstant match {
        case Some(n: Number) => denominator.toConstant match {
            case Some(d: Number) => n ?*? d.inverse
            case _ => None
        }
        case _ => None
    }

    def variables = numerator.variables ++: denominator.variables

    def isEmpty = numerator.isEmpty

    override def df(x: Variable) = (numerator.df(x) / denominator) - (numerator * denominator.df(x) / (denominator ^ 2))

    /**
     * A fraction times something is a fraction.
     * @param that
     * @return
     */
    override def ?*(that: Expression)(leftToRight: Boolean): Option[Expression] = that match {
        case _ if denominator equals that => Some(numerator)
        case ef: ExpressionFraction => Some(this.?*(ef)(leftToRight))
        case _ => super.?*(that)(leftToRight)
    }

    protected def ?*(that: ExpressionFraction)(leftToRight: Boolean): Expression = {
        if (leftToRight) (this.numerator * that.numerator) / (this.denominator * that.denominator)
        else (that.numerator * this.numerator) / (that.denominator * numerator)
    }

    override def toString = s"($numerator/$denominator)"

}

class ExpressionPower(val base: Expression, val power: Expression)
        extends Expression {

    def replace(variables: Map[Variable, Expression]) = base.replace(variables) ^ power.replace(variables)

    def toConstant = base.toConstant match {
        case Some(n) => power.toConstant match {
            case Some(m) => n ?^ m
            case _ => None
        }
        case _ => None
    }

    def variables = base.variables ++: power.variables

    def isEmpty = base.isEmpty

    override def ^(x: Expression) = base ^ (power + x)

    override def df(x: Variable) = {
        (base ^ (power - 1)) * ((base.df(x) * power) + (base * Ln(base) * power.df(x)))
    }

    override def toString = s"($base ^ $power)"

}

trait Nonvariate
        extends Expression {

    def variables = Set()

    //def /(that: Univariate): Univariate = ???

}

object Univariate {

    implicit def convert(expression: Expression): Univariate = expression match {
        case n: Number => new NonvariateWrapper(n)
        case u: Univariate => u
        case _ if (expression.variables.size == 1) => new UnivariateWrapper(expression)
        case _ => ???
    }

    private class NonvariateWrapper(val n: Number)
            extends AnyRef
            with Univariate {

        def replace(variables: Map[Variable, Expression]) = this

        def toConstant: Option[Number] = n.toConstant

        def isEmpty = n.isEmpty

        override def df(x: Variable) = Zero

        def variable = ???

        //override def apply(n: N):N = this.n

        override def dx = Zero

        override def toString = n.toString

    }

    private class UnivariateWrapper(val expression: Expression)
            extends AnyRef
            with Univariate {

        require(expression.variables.size == 1, "Require 1 variable but " + expression + " had " + expression.variables)

        def replace(variables: Map[Variable, Expression]) = expression.replace(variables)

        def toConstant = expression.toConstant

        def isEmpty = expression.isEmpty

        def variable = expression.variables.iterator.next()

        override def +(that: Expression) = expression + that

        override def ?*(that: Expression)(leftToRight: Boolean) = expression.?*(that)(leftToRight)

        override def /(that: Expression) = expression / that

        override def ^(that: Expression) = expression ^ that

        override def df(x: Variable) = expression.df(x)

        override def toString = expression.toString

    }

}

trait Univariate
        extends Expression {

    def variable: Variable

    def variables = Set(variable)

    def apply[R <: Number](n: Number)(implicit conversion: NumberIdentityArithmetic[R]): R = {
        val out: Number = replace(variable, n).toConstant.get
        conversion.convert(out).get
    }

    def apply(u: Univariate): Univariate = replace(variable, u)

    def dx: Univariate = df(variable)

}
