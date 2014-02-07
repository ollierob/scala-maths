package net.ollie.maths.geometry

import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers.PositiveReal

/**
 * Created by Ollie on 19/01/14.
 */
trait Quadrilateral
        extends Polygon {

    final def sides = 4

}

object Quadrilateral {

}

/**
 * @see http://mathworld.wolfram.com/Trapezoid.html
 */
object Trapezoid {

    def area(side1: PositiveReal, side2: PositiveReal, h: PositiveReal): PositiveReal = {
        h * (side1 + side2) / 2
    }

}

class Trapezoid(a: PositiveReal, b: PositiveReal, h: PositiveReal)
        extends Quadrilateral {

    lazy val g: PositiveReal = PositiveSquareRoot((a - b).squared + h.squared)

    def area = Trapezoid.area(a, b, h)

    def perimeter = a + b + h + g

}
