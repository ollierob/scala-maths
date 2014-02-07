package net.ollie.maths.geometry

import net.ollie.maths.numbers.{Real, PositiveReal}
import net.ollie.maths.functions.numeric.PositiveSquareRoot
import net.ollie.maths.functions.angular._
import Angle._
import net.ollie.utils.Homogeneous3Tuple

/**
 * Created by Ollie on 06/02/14.
 */
trait Triangle
        extends Polygon {

    final def sides = 3

    type AngleOpposite = (Angle, PositiveReal)

    def describe: Homogeneous3Tuple[AngleOpposite]

    def perimeter = describe.iterator.map(_._2).sum(Real.RealArithmetic).abs

    def isRight: Boolean = describe.iterator.find(_._1 == RightAngle).isDefined

    def inradius: PositiveReal = area / semiperimeter

    override def toString = s"Triangle($describe)"

}

object Triangle {

    val SIXTY_DEGREES = 60 degrees

    def apply(a: PositiveReal, b: PositiveReal, c: PositiveReal): Scalene = {
        classify(a, b, c) match {
            case Classification.EQUILATERAL => Triangle(a)
            case _ => new ScaleneSides(a, b, c)
        }
    }

    def apply(side: PositiveReal): Equilateral = new EquilateralSide(side)

    def apply(shared: PositiveReal, sharedAngle: Angle): Isosceles = {
        if (sharedAngle == SIXTY_DEGREES) Triangle(shared)
        new IsoscelesSideAndAngle(shared, sharedAngle)
    }

    private object Classification extends Enumeration {

        type Classification = Value
        val EQUILATERAL, ISOSCELES, SCALENE = Value

    }

    import Classification._

    private def classify(a: PositiveReal, b: PositiveReal, c: PositiveReal): Classification = {
        val b1 = a == b
        val b2 = b == c
        if (b1 && b2) return Classification.EQUILATERAL
        val b3 = a == c
        if ((b1 && b3) || (b2 && b3)) return Classification.ISOSCELES
        else return Classification.SCALENE
    }

}

trait Scalene
        extends Triangle

trait Isosceles
        extends Scalene

trait Equilateral
        extends Isosceles {

    def side: PositiveReal

    override def toString = s"Equilateral($side)"

}

/**
 *
 * @param side
 * @see http://mathworld.wolfram.com/EquilateralTriangle.html
 */
private class EquilateralSide(val side: PositiveReal)
        extends Equilateral {

    val PAIR = (Triangle.SIXTY_DEGREES, side)

    def describe = (PAIR, PAIR, PAIR)

    override def perimeter: PositiveReal = 3 * side

    def altitude = PositiveSquareRoot(3) * side / 2

    def area = altitude * side / 2

    override def inradius = PositiveSquareRoot(3) * side / 6

    def circumradius = 2 * inradius

    override def isRight = false

}

/**
 * At least two sides of equal length.
 * @param sharedSide
 * @param sharedAngle
 * @see http://mathworld.wolfram.com/IsoscelesTriangle.html
 */
private class IsoscelesSideAndAngle(val sharedSide: PositiveReal, val sharedAngle: Angle)
        extends Isosceles {

    require(sharedAngle < RightAngle)

    val uniqueSide: PositiveReal = (2 * sharedSide * Sin(sharedAngle / 2)).abs

    val uniqueAngle: Angle = (HalfAngle - sharedAngle) / 2

    private lazy val described: Homogeneous3Tuple[AngleOpposite] = ((sharedAngle, sharedSide), (sharedAngle, sharedSide), (uniqueAngle, uniqueSide))

    def describe = described

    def height = PositiveSquareRoot(sharedSide.squared - (uniqueSide.squared / 4)).get

    override def area = uniqueSide * height / 2

}

private class ScaleneSides(val a: PositiveReal, val b: PositiveReal, val c: PositiveReal)
        extends Scalene {

    val alpha: Angle = ArcCos((b.squared + c.squared - a.squared) / (2 * b * c))

    val beta: Angle = ArcCos((a.squared + c.squared - b.squared) / (2 * a * c))

    val gamma: Angle = HalfAngle - alpha - beta

    private lazy val described: Homogeneous3Tuple[AngleOpposite] = ((alpha, a), (beta, b), (gamma, c))

    def describe = described

    private val s = semiperimeter

    def area = PositiveSquareRoot(s * (s - a) * (s - b) * (s - c)).get

}