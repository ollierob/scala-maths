package net.ollie.maths.geometry

import net.ollie.maths.numbers.{Natural, PositiveReal}
import net.ollie.maths.functions.angular.{Sec, Angle, Cotan}
import net.ollie.maths.numbers.constants.Pi
import Angle._

/**
 * Created by Ollie on 19/01/14.
 * @see http://mathworld.wolfram.com/Polygon.html
 */
trait Polygon {

    def sides: Natural

    def area: PositiveReal

    def perimeter: PositiveReal

    def semiperimeter: PositiveReal = perimeter / 2

}

/**
 * @see http://mathworld.wolfram.com/RegularPolygon.html
 */
trait RegularPolygon
        extends Polygon {

    def sideLength: PositiveReal

    def inradius: PositiveReal = sideLength * Cotan(Pi / sides radians).abs / 2

    def circumradius: PositiveReal = inradius * Sec(Pi / sides radians).abs

    def area: PositiveReal = sides * sideLength * inradius / 2

    def perimeter: PositiveReal = sideLength * sides

    def apothem: PositiveReal = 2 * area / perimeter

}
