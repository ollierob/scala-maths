package net.ollie.maths

import net.ollie.maths.numbers.constants.{One, Zero}

/**
 * Created by Ollie on 09/03/14.
 * @see http://mathworld.wolfram.com/LinearEquation.html
 * @see [[net.ollie.maths.equations.LinearEquation]]
 */
trait Linear
        extends Univariate {

    def left: Constant

    def right: Constant

    def isEmpty = left.isEmpty || right.isEmpty

    def toConstant = None

    def unary_-(): Linear = Linear.negate(this)

    override def ?*(that: Expression)(leftToRight: Boolean) = {
        that.toConstant match {
            case Some(n) => Some(this.*(n)(leftToRight))
            case _ => super.?*(that)(leftToRight)
        }
    }

    def *(that: Constant)(leftToRight: Boolean): Univariate = {
        if (leftToRight) Linear(that * left, variable, right)
        else Linear(left, variable, right * that)
    }

    def /(that: Constant): Univariate = {
        Linear(left, variable, right * that.inverse)
    }

    def df(x: Variable): Univariate = {
        if (x == variable) left * right
        else Zero
    }

}

object Linear {

    def apply(left: Constant, variable: Variable, right: Constant): Univariate = {
        (left, right) match {
            case (Zero, _) => Zero
            case (_, Zero) => Zero
            case (One, One) => variable
            case (One, _) => new RightLinearTerm(variable, right)
            case (_, One) => new LeftLinearTerm(left, variable)
            case _ => new LinearTerm(left, variable, right)
        }
    }

    def multiply(variable: Variable, constant: Constant)(implicit leftToRight: Boolean): Univariate = {
        if (leftToRight) Linear(constant, variable, One)
        else Linear(One, variable, constant)
    }

    def divide(variable: Variable, denominator: Constant): Univariate = {
        Linear(One, variable, denominator.inverse)
    }

    def negate(linear: Linear): Linear = {
        new NegatedLinear(linear)
    }

}

private class LeftLinearTerm(override val left: Constant, override val variable: Variable)
        extends LinearTerm(left, variable, One) {

    override def toString = s"($left * $variable)"

}

private class RightLinearTerm(override val variable: Variable, override val right: Constant)
        extends LinearTerm(One, variable, right) {

    override def toString = s"($variable * $right)"

}

private class LinearTerm(val left: Constant, val variable: Variable, val right: Constant)
        extends Linear {

    def replace(variables: Map[Variable, Expression]) = variables.get(variable) match {
        case Some(x) => left * x * right
        case _ => this
    }

    override def toString = s"($left * $variable * $right)"

}

private class NegatedLinear(val of: Linear)
        extends Linear {

    def variable = of.variable

    def left = -(of.left)

    def right = of.right

    def replace(variables: Map[Variable, Expression]) = -(of.replace(variables))

}
