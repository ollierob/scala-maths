package net.ollie.maths.functions

import net.ollie.maths._
import net.ollie.maths.numbers.Zero

/**
 * Created by Ollie on 16/01/14.
 */
trait DifferentiableUnivariate
        extends Differentiable
        with Univariate {

    override def unary_-(): DifferentiableUnivariate = new DifferentiableNegatedExpression(this) with DifferentiableUnivariate {

        override def unary_-() = DifferentiableUnivariate.this

        override def variable: Variable = DifferentiableUnivariate.this.variable

        override def variables = super[DifferentiableUnivariate].variables

        override def df(x: Variable): DifferentiableUnivariate = -(DifferentiableUnivariate.this.df(x))

        override def toString = "-(" + DifferentiableUnivariate.this.toString + ")"

    }

    def df(x: Variable): DifferentiableUnivariate

    def dx: DifferentiableUnivariate = df(variable)

}

object DifferentiableUnivariate {

    implicit def convert(x: Variable): DifferentiableUnivariate = new DifferentiableUnivariate {

        def df(x: Variable): DifferentiableUnivariate = convert(x.df(x), x)

        def replace(variables: Map[Variable, Expression]) = x.replace(variables)

        def toConstant = x.toConstant

        def isEmpty = x.isEmpty

        def variable = x

    }

    implicit def convert(expr: Differentiable): DifferentiableUnivariate = {
        if (expr.variables.size != 1) ???
        new DifferentiableUnivariateWrapper(expr)
    }

    def convert(n: Number, x: Variable): DifferentiableUnivariate = new NumericUnivariate(n, x)

    private class NumericUnivariate(val n: Number, val x: Variable)
            extends DifferentiableUnivariate {

        def df(x: Variable) = convert(Zero, x)

        def replace(variables: Map[Variable, Expression]) = n.replace(variables)

        def toConstant = n.toConstant

        def isEmpty = n.isEmpty

        def variable = x

    }

    private class DifferentiableUnivariateWrapper(val expression: Differentiable)
            extends DifferentiableUnivariate {

        require(expression.variables == Set(variable))

        def variable = expression.variables.iterator.next

        def df(x: Variable) = expression.df(x) match {
            case du: DifferentiableUnivariate => du
            case otherwise => new DifferentiableUnivariateWrapper(otherwise)
        }

        def replace(variables: Map[Variable, Expression]) = expression.replace(variables)

        def toConstant = expression.toConstant

        def isEmpty = expression.isEmpty

        override def toString = expression.toString

    }

}
