package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.expressions.{Empty, Expression, NegatedExpression}
import net.ollie.maths.functions.Represented
import net.ollie.maths.functions.numeric.Roots
import net.ollie.maths.numbers.Natural
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 08/01/14.
 *
 * @see http://mathworld.wolfram.com/Polynomial.html
 */
trait Polynomial
    extends Represented {

    def degree: Natural

    def of: Expression

    override def representation: Expression

    override def unary_-(): Polynomial = Polynomial.negate(this)

    override def toString = representation.toString

}

object Polynomial {

    def negate[P <: Polynomial](p: P): NegatedPolynomial[P] = new NegatedPolynomial[P](p)

    def apply(x: Variable) = apply(x, 0)

    def apply(x: Variable, z: Complex): SingleVariablePolynomial = new ConstantSingleVariablePolynomial(x, z)

    def apply(x: Variable, xCoeff: Complex, c: Complex) = LinearPolynomial.apply(x, xCoeff, c)

    def apply(x: Variable, xSquaredCoeff: Complex, xCoeff: Complex, c: Complex) = QuadraticPolynomial.apply(x, xSquaredCoeff, xCoeff, c)

}

class NegatedPolynomial[P <: Polynomial](override val of: P)
    extends NegatedExpression(of)
        with Polynomial {

    override def representation = -(of.representation)

    override def degree = of.degree

    override def unary_-(): P = of

    override def variables = super[Polynomial].variables

    override def replace(variables: Map[Variable, Expression]) = super[NegatedExpression].replace(variables)

    override def toConstant = super[NegatedExpression].toConstant

    override def isEmpty = super[Polynomial].isEmpty

    override def df(x: Variable) = super[NegatedExpression].df(x)

}

trait EmptyPolynomial
    extends Polynomial
        with Empty {

    def representation: Expression with Empty = Zero

    override def unary_-(): Polynomial with Empty = this

    override def toConstant = super[Empty].toConstant

    override def df(x: Variable) = this

    override def variables = super[Empty].variables

    override def isEmpty = super[Empty].isEmpty

}

abstract class ConstantPolynomial[N <: Constant](val representation: N)
    extends Polynomial

trait PolynomialRoots[+F <: Constant, C <: Constant]
    extends Roots[C] {

    override def of: Polynomial

    override def degree = of.degree

}

class ConstantSingleVariablePolynomial(val of: Variable, val c: Constant)
    extends SingleVariablePolynomial {

    override def degree = 0

    override def representation = c

    override def roots = ???

}