package net.ollie.maths.numbers.complex

/**
 * Created by Ollie on 04/01/14.
 */
class ComplexConjugate(val of: Complex)
        extends AnyRef
        with Complex {

    override def unary_-() = new ComplexConjugate(-of)

    override def conjugate = of

    def re = of.re

    def im = -(of.im)

    override def toString = s"($of)*"

    override def abs = of.abs

    override def arg = -of.arg

}
