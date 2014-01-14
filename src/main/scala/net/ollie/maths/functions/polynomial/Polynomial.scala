package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.functions.Represented
import net.ollie.maths.numbers.{NaturalNumber, Zero}

/**
 * Created by Ollie on 08/01/14.
 */
trait Polynomial
        extends Differentiable
        with Represented {

    def degree: NaturalNumber

    protected[this] def f: Differentiable

    def df(variable: Variable) = f.df(variable)

}

object ZeroPolynomial
        extends Polynomial
        with Empty {

    def degree = Zero

    protected[this] def f = Zero

    override def df(x: Variable) = this

    override def variables = super[Empty].variables

    override def isEmpty = super[Empty].isEmpty

}
