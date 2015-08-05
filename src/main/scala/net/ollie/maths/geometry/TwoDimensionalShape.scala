package net.ollie.maths.geometry

import net.ollie.maths.numbers.PositiveReal

/**
 * Created by Ollie on 09/02/14.
 */
trait TwoDimensionalShape {

    def area: PositiveReal

    def perimeter: PositiveReal

    def semiperimeter: PositiveReal = perimeter / 2

}
