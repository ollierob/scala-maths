package net.ollie.maths.functions.numeric

import net.ollie.maths.Constant
import net.ollie.maths.functions.angular.{Cos, Sin}
import net.ollie.maths.numbers.complex.{Complex, ImaginaryUnit, PolarComplex}
import net.ollie.maths.numbers.constants.Pi
import net.ollie.maths.numbers.{Natural, Real}

import scala.collection.mutable

trait NthRoots[+F <: Constant, C <: Constant]
    extends NumericRoots[F, C] {

}

object ComplexNthRoots {

    def apply(z: Complex, degree: Natural): Set[Complex] = {
        val polar = PolarComplex(z)
        val rP: Real = (polar.r ^ (1 / degree)).principal.re
        val set = mutable.Set[Complex]()
        for (k <- 1 until degree.requireInt) {
            val factor: Real = (polar.theta + (2 * Pi * k)) / degree
            set += rP * (Cos(factor) + ImaginaryUnit * Sin(factor))
        }
        set.toSet
    }

}