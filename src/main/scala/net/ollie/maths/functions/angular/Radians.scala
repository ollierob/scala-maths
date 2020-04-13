package net.ollie.maths.functions.angular

import net.ollie.maths.functions.angular.Angle.AngleBuilder
import net.ollie.maths.functions.numeric.Modulo
import net.ollie.maths.numbers.constants.{Pi, Zero}
import net.ollie.maths.numbers.{Integer, Precision, Real}
import net.ollie.utils.BigDecimals

trait Radians
    extends Angle {

    type Type = Radians

    def value: Real

    def toRadians = value

    def reduce = Radians(Modulo(value, 2 * Pi).remainder)

    def +(that: Radians): Radians = Radians(this.value + that.value)

    def -(that: Radians): Radians = Radians(this.value - that.value)

    override def unary_-(): Radians = Radians(-value)

    override def toString = s"rad($value)"

    implicit def builder = Radians.Builder

}

object Radians {

    implicit def apply(re: Real): Radians = re match {
        case Zero => EmptyAngle
        case Pi => OnePiRadians
        case rad: Radians => rad
        case angle: Angle => new KnownRadians(angle.toRadians)
        case _ => new KnownRadians(re)
    }

    implicit object Builder extends AngleBuilder[Radians] {

        def create(radians: Real) = Radians(radians)

    }

}

class KnownRadians protected[angular](val value: Real)
    extends Radians

case class PiRadians(n: Integer)
    extends Radians {

    override def value = n * Pi

    override def evaluate(precision: Precision): BigDecimal = BigDecimals.pi(precision) * n.evaluate(precision)

}

private object OnePiRadians
    extends PiRadians(1)