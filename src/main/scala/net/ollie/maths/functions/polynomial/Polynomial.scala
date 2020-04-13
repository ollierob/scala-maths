package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.expressions.{Empty, Expression, NegatedExpression}
import net.ollie.maths.functions.Represented
import net.ollie.maths.functions.numeric.Roots
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.Zero

/**
 * Finite-length power series.
 *
 * Created by Ollie on 08/01/14.
 *
 * @see http://mathworld.wolfram.com/Polynomial.html
 */
trait Polynomial
    extends PowerSeries with Represented {

    def degree: Natural

    def of: Expression

    override def representation: Expression = {
        Series((n: Natural) => coefficient(n) * (of ^ n), Zero, degree)
    }

    override def unary_-(): Polynomial = Polynomial.negate(this)

    override def toString = representation.toString

    override def equals(expr: Expression) = expr match {
        case p: Polynomial => representation == p.representation
        case _ => super.equals(expr)
    }
}

object Polynomial {

    def negate[P <: Polynomial](p: P): NegatedPolynomial[P] = new NegatedPolynomial[P](p)

    def apply(x: Variable): UnivariatePolynomial = apply(x, 0)

    //TODO remove, replace with non-variate
    def apply(x: Variable, z: Complex): UnivariatePolynomial = new ConstantUnivariatePolynomial(x, z)

    def apply(x: Variable, xCoeff: Complex, c: Complex): UnivariatePolynomial = LinearPolynomial(x, xCoeff, c)

    def apply(x: Variable, xSquaredCoeff: Complex, xCoeff: Complex, c: Complex): UnivariatePolynomial = QuadraticPolynomial(x, xSquaredCoeff, xCoeff, c)

    def apply(x: Variable, coefficients: Seq[Complex]): UnivariatePolynomial = NthDegreePolynomial(x, coefficients)

}

class NegatedPolynomial[P <: Polynomial](override val expression: P)
    extends NegatedExpression[P](expression)
        with Polynomial {

    override type Coefficient = Constant

    override def of = expression.of

    override def representation = -(expression.representation)

    override def degree = expression.degree

    override def unary_-(): P = expression

    override def variables = super[Polynomial].variables

    override def replace(variables: Map[Variable, Expression]) = super[NegatedExpression].replace(variables)

    override def coefficient(power: Natural) = -expression.coefficient(power)

    override def toConstant = super[NegatedExpression].toConstant

    override def isEmpty = super[Polynomial].isEmpty

    override def df(x: Variable): Expression = super[NegatedExpression].df(x)

}

trait EmptyPolynomial
    extends Polynomial
        with Empty {

    override type Coefficient = Zero.type

    override def representation: Expression with Empty = Zero

    override def unary_-(): Polynomial with Empty = this

    override def toConstant = super[Empty].toConstant

    override def coefficient(power: Natural) = Zero

    override def df(x: Variable) = this

    override def variables = super[Empty].variables

    override def isEmpty = super[Empty].isEmpty

}

abstract class ConstantPolynomial[N <: Constant](override val representation: N)
    extends Polynomial

trait PolynomialRoots[+F <: Constant, C <: Constant]
    extends Roots[C] {

    override def of: Polynomial

    override def degree = of.degree

}