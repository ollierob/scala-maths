package net.ollie.maths.numbers.complex

import net.ollie.maths.numbers.surreal.{EmptyForm, Surreal}
import net.ollie.maths.Empty
import net.ollie.maths.numbers.Real
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 06/01/14.
 */
trait Surcomplex
        extends Complex {

    def re: Surreal

    def im: Surreal

    override def toReal: Option[Surreal] = if (im.isEmpty) Some(re) else None

    override def unary_-(): Surcomplex = Surcomplex(super.unary_-())

    override def conjugate: Surcomplex = Surcomplex(super.conjugate)

    override def +(that: Complex): Surcomplex = Surcomplex(super.+(that))

    override def -(that: Complex): Surcomplex = Surcomplex(super.-(that))

    override def *(that: Complex): Surcomplex = Surcomplex(super.*(that))

    override def /(that: Complex): Surcomplex = Surcomplex(super./(that))

}

object Surcomplex {

    implicit def apply(z: Complex): Surcomplex = z match {
        case s: Surcomplex => s
        case _ => new RegularSurcomplex(z.re, z.im)
    }

    implicit def apply(re: Real): Surcomplex = new RegularSurcomplex(re, Zero)

    def apply(re: Surreal, im: Surreal): Surcomplex = new RegularSurcomplex(re, im)

}

class RegularSurcomplex(val re: Surreal, val im: Surreal)
        extends Surcomplex

object Surimaginary {

    implicit def apply(re: Real): Surimaginary = new Surimaginary(re)

    implicit def apply(i: Imaginary): Surimaginary = i match {
        case s: Surimaginary => s
        case _ => new Surimaginary(i.coefficient)
    }

    def apply(s: Surreal): Surimaginary = new Surimaginary(s)

}

class Surimaginary(override val coefficient: Surreal)
        extends Imaginary(coefficient)
        with Surcomplex {

    override def re: Surreal with Empty = EmptyForm

    override def im: Surreal = coefficient

    override def unary_-(): Surimaginary = Surimaginary(-coefficient)

    override def conjugate: Surimaginary = Surimaginary(super[Imaginary].conjugate)

}