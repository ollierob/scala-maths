package net.ollie.maths.geometry

import net.ollie.maths.numbers.PositiveRealNumber

/**
 * Created by Ollie on 14/01/14.
 */
trait Shape {

    def area: PositiveRealNumber

    def perimeter: PositiveRealNumber

    def semiperimeter: PositiveRealNumber = perimeter / 2

}
