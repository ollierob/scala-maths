package net.ollie.maths.numbers.complex


/**
 * Created by Ollie on 04/01/14.
 */
class ComplexConjugate(val of: ComplexNumber)
        extends AnyRef
        with ComplexNumber {

    override def unary_-() = new ComplexConjugate(-of)

    override def conjugate = of

    def re = of.re

    def im = -(of.im)

    override def toString = "(" + of + ")*"

    def abs = of.abs

    def arg = -of.arg

}
