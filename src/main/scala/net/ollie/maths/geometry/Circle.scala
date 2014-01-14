package net.ollie.maths.geometry

import net.ollie.maths.numbers.PositiveRealNumber
import net.ollie.maths.numbers.real.Pi

/**
 * Created by Ollie on 14/01/14.
 */
class Circle(val radius: PositiveRealNumber)
        extends Shape {

    def diameter: PositiveRealNumber = 2 * radius

    def area: PositiveRealNumber = 2 * Pi * radius

    def circumference: PositiveRealNumber = Pi * radius

    def perimeter = circumference

}

object Circle {

    def apply(radius: PositiveRealNumber): Circle = new Circle(radius)

    def withDiameter(d: PositiveRealNumber): Circle = new Circle(d / 2)

}
