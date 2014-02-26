package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers._
import net.ollie.maths.{IdentityArithmetic, EmptyNumber, Number}
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.{Unity, Pi, One, Zero}
import scala.collection.mutable.ArrayBuffer

/**
 * Created by Ollie on 12/02/14.
 */
object Roots {

    def apply(of: Real, degree: Natural): Roots[Real, Complex] = of match {
        case Zero => new EmptyRoots[Real, Complex](Zero, degree)
        case One => Roots.unity(degree)
        case _ => new NonEmptyRoots(of, degree)
    }

    def unity(degree: Natural): Roots[Unity, Complex] = new RootsOfUnity(degree)

}

/**
 *
 * @tparam F type of the number we are taking the roots of.
 * @tparam C type of the roots.
 */
trait Roots[+F <: Number, C <: Number]
        extends Multivalued {

    type Contents = C

    def of: F

    def principal: C

    def values: Set[C]

    def isEmpty = values.forall(_.isEmpty)

    def degree: Natural

    override def toString = s"Root($degree)($of)"

}

class EmptyRoots[+F <: EmptyNumber, C <: Number](val of: F, val degree: Natural)
        (implicit conversion: IdentityArithmetic[F, C])
        extends Roots[F, C] {

    def inverse = ??? //TODO

    def principal = conversion.promote(of)

    def values = Set(principal)

}

abstract class FirstRoot[F <: Number](val principal: F)
        extends Roots[F, F] {

    def degree = One

    def values = Set(principal)

}

class NonEmptyRoots(val of: Real, val degree: Natural)
        extends Roots[Real, Complex] {

    def inverse = Roots(of.inverse, degree)

    private lazy val complexPrincipal: Complex = PrincipalRoot(of.abs, degree) * PrincipalRoot(Signum(of), degree)

    private lazy val complexValues: Set[Complex] = Roots.unity(degree).values.map(_ * complexPrincipal)

    def principal = complexPrincipal

    def values = complexValues

}

/**
 *
 * @param degree
 * @see http://mathworld.wolfram.com/RootofUnity.html
 */
class RootsOfUnity(val degree: Natural)
        extends Roots[One.type, Complex] {

    import net.ollie.maths.numbers.complex.{ImaginaryUnit => i}

    def inverse = this

    def of = One

    def principal = One

    private lazy val vals: Set[Complex] = {
        val n = degree.toInt.get
        val roots = new ArrayBuffer[Complex](n)
        for (k <- 1 to n) {
            roots += Exp((2 * k) * i * Pi / degree)
        }
        roots += principal
        roots.toSet
    }

    def values = vals

    override def toString = s"RootsOfUnity($degree)"

}