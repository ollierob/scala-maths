package net.ollie.maths.functions.numeric

import net.ollie.maths.numbers._
import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.{One, Pi, Unity, Zero}
import net.ollie.maths.{Constant, Expression, NumberConversionArithmetic}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by Ollie on 12/02/14.
 */
object NumericRoots {

    /**
     * The positive root of a real number has complex values.
     *
     * @param of
     * @param degree
     * @return
     */
    def apply(of: Real, degree: Natural): NumericRoots[Real, Complex] = of match {
        case Zero => new EmptyRoots[Zero.type, Complex](Zero, degree)
        case One => NumericRoots.unity(degree)
        case _ => new NonEmptyRoots(of, degree)
    }

    def unity(degree: Natural): NumericRoots[Real with Unity, Complex] = new RootsOfUnity(degree)

}

/**
 * @tparam C type of the roots.
 */
trait Roots[C <: Constant]
    extends Multivalued {

    type Contents = C

    def of: Expression

    def principal: C

    def values: Set[C]

    def isEmpty = values.isEmpty || values.forall(_.isEmpty)

    def degree: Natural

    override def inverse: Constant = ??? //TODO

    override def toString = s"Roots($degree)($of)"

}

/**
 *
 * @tparam F type of the number we are taking the roots of.
 * @tparam C type of the roots.
 */
trait NumericRoots[+F <: Constant, C <: Constant]
    extends Roots[C] {

    override def of: F

}

class EmptyRoots[+F <: EmptyConstant, C <: Constant](val of: F, val degree: Natural)
                                                    (implicit conversion: NumberConversionArithmetic[F, C])
    extends NumericRoots[F, C] {

    override def inverse = ??? //TODO

    def principal = conversion.apply(of)

    def values = Set(principal)

}

abstract class FirstRoot[F <: Constant](val principal: F)
    extends NumericRoots[F, F] {

    def degree = One

    def values = Set(principal)

}

class NonEmptyRoots(val of: Real, val degree: Natural)
    extends NumericRoots[Real, Complex] {

    override def inverse = NumericRoots(of.inverse, degree)

    val principal: Complex = {
        PrincipalRoot(of.abs, degree) * PrincipalRoot(Signum(of), degree)
    }

    val values: Set[Complex] = {
        NumericRoots.unity(degree).values.map(_ * principal)
    }

}

/**
 *
 * @param degree
 * @see http://mathworld.wolfram.com/RootofUnity.html
 */
class RootsOfUnity(val degree: Natural)
    extends NumericRoots[One.type, Complex] {

    import net.ollie.maths.numbers.complex.{ImaginaryUnit => i}

    override def inverse = this

    def of = One

    def principal = One

    val values: Set[Complex] = {
        val n = degree.toInt.get
        val roots = new ArrayBuffer[Complex](n)
        for (k <- 1 to n) {
            roots += Exp((2 * k) * i * Pi / degree)
        }
        roots += principal
        roots.toSet
    }

    override def toString = s"RootsOfUnity($degree)"

}