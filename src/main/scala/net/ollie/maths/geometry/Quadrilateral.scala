package net.ollie.maths.geometry

import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers.PositiveReal
import net.ollie.utils.Homogeneous4Tuple

/**
 * Created by Ollie on 19/01/14.
 */
trait Quadrilateral
        extends Polygon {

    final def sides = 4

    def perimeter = describeSides.iterator.sum

    def describeSides: Homogeneous4Tuple[PositiveReal]

}

/**
 * @see http://mathworld.wolfram.com/Trapezoid.html
 */
object Trapezoid {

    def area(side1: PositiveReal, side2: PositiveReal, h: PositiveReal): PositiveReal = {
        h * (side1 + side2) / 2
    }

}

/**
 * Not enough information to fully describe.
 * @param a
 * @param b
 * @param h
 */
abstract class Trapezoid(val a: PositiveReal, val b: PositiveReal, val h: PositiveReal)
        extends Quadrilateral {

    private lazy val g: PositiveReal = PositiveSquareRoot((a - b).squared + h.squared)

    override def perimeter = a + b + h + g

    def area = Trapezoid.area(a, b, h)

}

class Square(val side: PositiveReal)
        extends Quadrilateral {

    def area = side.squared

    def describeSides = (side, side, side, side)

    override def toString = s"Square($side)"

}