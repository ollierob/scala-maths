package net.ollie.maths.geometry

import net.ollie.maths.numbers.NaturalNumber

/**
 * Created by Ollie on 14/01/14.
 * @see http://mathworld.wolfram.com/Polygon.html
 */
trait Polygon
        extends Shape {

    def numSides: NaturalNumber

}
