package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.functions.Represented
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 08/01/14.
 * @see http://mathworld.wolfram.com/Polynomial.html
 */
trait Polynomial
        extends Represented {

    override def unary_-(): Polynomial = new NegatedPolynomial(this)

}

class NegatedPolynomial(override val of: Polynomial)
        extends NegatedExpression(of)
        with Polynomial {

    def representation = -(of.representation)

    override def variables = super[Polynomial].variables

    override def replace(variables: Map[Variable, Expression]) = super[NegatedExpression].replace(variables)

    override def toConstant = super[NegatedExpression].toConstant

    override def isEmpty = super[Polynomial].isEmpty

    override def df(x: Variable) = super[NegatedExpression].df(x)

}

object ZeroPolynomial
        extends Polynomial
        with Empty {

    def representation = Zero

    override def unary_-(): Polynomial with Empty = this

    override def toConstant = super[Empty].toConstant

    override def df(x: Variable) = this

    override def variables = super[Empty].variables

    override def isEmpty = super[Empty].isEmpty

}

abstract class ConstantPolynomial[N <: Number](val representation: N)
        extends Polynomial
