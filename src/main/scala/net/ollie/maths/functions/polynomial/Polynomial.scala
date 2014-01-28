package net.ollie.maths.functions.polynomial

import net.ollie.maths._
import net.ollie.maths.functions.Represented
import net.ollie.maths.numbers.Zero

/**
 * Created by Ollie on 08/01/14.
 */
trait Polynomial
        extends Represented

object ZeroPolynomial
        extends Polynomial
        with Empty {

    protected[this] def f = Zero

    override def toConstant = super[Empty].toConstant

    override def df(x: Variable) = this

    override def variables = super[Empty].variables

    override def isEmpty = super[Empty].isEmpty

}
