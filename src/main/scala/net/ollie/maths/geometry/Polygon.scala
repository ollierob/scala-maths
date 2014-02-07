package net.ollie.maths.geometry

import net.ollie.maths.numbers.{Natural, PositiveReal}

/**
 * Created by Ollie on 19/01/14.
 * @see http://mathworld.wolfram.com/Polygon.html
 */
trait Polygon
        extends Shape {

    def sides: Natural

    def area: PositiveReal

    def perimeter: PositiveReal

    def semiperimeter: PositiveReal = perimeter / 2

}
