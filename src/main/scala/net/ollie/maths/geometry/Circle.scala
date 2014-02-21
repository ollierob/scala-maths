package net.ollie.maths.geometry

import net.ollie.maths.numbers.PositiveReal
import net.ollie.maths.numbers.constants.Pi
import net.ollie.maths.functions.numeric.PositiveSquareRoot

/**
 * Created by Ollie on 09/02/14.
 */
object Circle {

    def withRadius(radius: PositiveReal): Circle = new Circle(radius)

    def withDiameter(diameter: PositiveReal): Circle = withRadius(diameter / 2)

}

class Circle(val radius: PositiveReal)
        extends Shape {

    def diameter: PositiveReal = 2 * radius

    def area: PositiveReal = Pi * radius.squared

    def circumference: PositiveReal = 2 * Pi * radius

    def perimeter: PositiveReal = circumference

    /**
     * @return a square with the same area as this circle.
     */
    def square: Square = new Square(PositiveSquareRoot(area))

    override def toString = s"Circle($radius)"

}
