package net.ollie.maths.geometry

import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.numbers.PositiveRealNumber

/**
 * Created by Ollie on 19/01/14.
 */
trait Quadrilateral
        extends Polygon {

}

object Quadrilateral {

}

/**
 * @see http://mathworld.wolfram.com/Trapezoid.html
 */
object Trapezoid {

    def area(side1: PositiveRealNumber, side2: PositiveRealNumber, h: PositiveRealNumber): PositiveRealNumber = {
        (side1 + side2) / (2 * h)
    }

}

class Trapezoid(a: PositiveRealNumber, b: PositiveRealNumber, h: PositiveRealNumber)
        extends Quadrilateral {

    lazy val g: PositiveRealNumber = PositiveSquareRoot((a - b).squared + h.squared)

    def area = Trapezoid.area(a, b, h)

    def perimeter = a + b + h + g

}
