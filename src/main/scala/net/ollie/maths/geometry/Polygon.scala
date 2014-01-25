package net.ollie.maths.geometry

import net.ollie.maths.numbers.PositiveReal

/**
 * Created by Ollie on 19/01/14.
 */
trait Polygon
        extends Shape {

    def area: PositiveReal

    def perimeter: PositiveReal

    def semiperimeter: PositiveReal = perimeter / 2

}
