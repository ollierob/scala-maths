package net.ollie.maths.functions.polynomial

import net.ollie.maths.expressions.{Expression, Univariate}
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.constants.Zero
import net.ollie.maths.{Constant, Variable}
import net.ollie.utils.Is

trait UnivariatePolynomial
    extends Polynomial with MaclaurinSeries with Univariate {

    def degree: Natural

    override def representation: Expression = {
        var expr: Expression = 0
        for (i <- 0 to degree.requireInt) {
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

    override def dx: UnivariatePolynomial

    override def equals(expr: Expression): Boolean = expr match {
        case u: UnivariatePolynomial => this equals u
        case _ => super.equals(expr)
    }

    def equals(that: UnivariatePolynomial): Boolean = {
        if (degree != that.degree) return false;
        for (i <- 0 until degree.requireInt) {
            if (this.coefficient(i) != that.coefficient(i)) return false
        }
        true
    }

}

object UnivariatePolynomial extends Is[Polynomial] {

    def is(poly: Polynomial) = poly.variables.size == 1

}

class ConstantUnivariatePolynomial(val of: Variable, val c: Constant)
    extends UnivariatePolynomial {

    override def degree = 0

    override lazy val representation = c

    override def coefficient(power: Natural) = power match {
        case Zero => c
        case _ => Zero
    }

    override def roots = ???

    override def dx = Polynomial(of) //FIXME technically should not have any variable

}