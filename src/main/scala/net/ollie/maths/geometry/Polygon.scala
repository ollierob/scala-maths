package net.ollie.maths.geometry

import net.ollie.maths.functions.angular.Angle._
import net.ollie.maths.functions.angular.{Angle, CoTan, Sec}
import net.ollie.maths.numbers.constants.Pi
import net.ollie.maths.numbers.{Natural, PositiveReal}

/**
 * Created by Ollie on 19/01/14.
 * @see http://mathworld.wolfram.com/Polygon.html
 */
trait Polygon
        extends TwoDimensionalShape {

    def sides: Natural

}

/**
 * @see http://mathworld.wolfram.com/RegularPolygon.html
 */
trait RegularPolygon
        extends Polygon {

    def sideLength: PositiveReal

    def inCircle: Circle = Circle.withRadius(inRadius)

    def inRadius: PositiveReal = sideLength * CoTan(Pi / sides radians).abs / 2

    def circumCircle = Circle.withRadius(circumRadius)

    def circumRadius: PositiveReal = inRadius * Sec(Pi / sides radians).abs

    def area: PositiveReal = sides * sideLength * inRadius / 2

    def perimeter: PositiveReal = sideLength * sides

    def apothem: PositiveReal = 2 * area / perimeter

}
