package net.ollie.maths.geometry

import net.ollie.maths.numbers.PositiveRealNumber

/**
 * Created by Ollie on 19/01/14.
 */
trait Polygon
        extends Shape {

    def area: PositiveRealNumber

    def perimeter: PositiveRealNumber

    def semiperimeter: PositiveRealNumber = perimeter / 2

}
