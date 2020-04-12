package net.ollie.maths.functions.polynomial

import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.numbers.Natural
import net.ollie.maths.{Constant, Variable}

trait UnivariatePolynomial
    extends Polynomial with Univariate {

    override def of: Variable

    override def variable = of

    override def variables = Set(of)

    def degree: Natural

    override def degreeOf(x: Variable) = {
        if (of == x) degree
        else 0
    }

    def coefficient(power: Natural): Constant

    override def representation: Expression = {
        var expr: Expression = 0
        for (i <- 0 to degree) {
            val coeff = coefficient(i)
            if (!coeff.isZero) {
                if (i == 0) expr += coeff
                else expr += coeff * (variable ^ i)
            }
        }
        expr
    }

    override def replace(variables: Map[Variable, Expression]): Expression = {
        if (variables.contains(of)) representation.replace(variables)
        else this
    }

    def replace(c: Constant): Constant = {
        replace(Map(of -> c)).toConstant.get
        //        var sum: Constant = Zero
        //        for (i <- 0 to degree) {
        //            val coeff = coefficient(i)
        //            if (!coeff.isZero) {
        //                if (i == 0) sum += coeff //FIXME need arithmetic
        //                else sum += coeff * (c ^ i)
        //            }
        //        }
        //        sum
    }

    def roots: PolynomialRoots[_, _]

    override def unary_-(): UnivariatePolynomial = ??? //TODO

    override def df(x: Variable): UnivariatePolynomial = {
        if (x == variable) derivative
        else Polynomial(x)
    }

    def derivative: UnivariatePolynomial

    override def equals(expr: Expression): Boolean = expr match {
        case u: UnivariatePolynomial => this equals u
        case _ => super.equals(expr)
    }

    def equals(that: UnivariatePolynomial): Boolean = {
        if (degree != that.degree) return false;
        for (i <- 0 until this.degree) {
            if (this.coefficient(i) != that.coefficient(i)) return false
        }
        true
    }

}

object UnivariatePolynomial {

    def is(poly: Polynomial) = poly.variables.size == 1

}