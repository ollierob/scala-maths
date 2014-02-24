package net.ollie.maths.functions.angular

import net.ollie.maths.Empty
import net.ollie.maths.functions.angular.Angle.AngleBuilder
import net.ollie.maths.numbers.{Precision, Real}
import net.ollie.maths.numbers.constants.{Zero, Pi}
import net.ollie.maths.functions.numeric.Modulo

trait Angle
        extends Real {

    type Type >: this.type <: Angle

    def toRadians: Real

    def isEmpty = toRadians.isEmpty

    def classify = {
        val rads: Real = toRadians.abs
        rads match {
            case _ if rads < (Pi / 2) => Classification.Acute
            case _ if rads == Pi / 2 => Classification.Right
            case _ if rads < Pi => Classification.Obtuse
            case Pi => Classification.Straight
            case _ if rads < 2 * Pi => Classification.Reflex
            case _ if rads == 2 * Pi => Classification.Full
            case _ => ??? //TODO peridocity
        }
    }

    def evaluate(precision: Precision) = toRadians.evaluate(precision)

    override def approximatelyEvaluate(precision: Precision) = toRadians.approximatelyEvaluate(precision)

    implicit def builder: AngleBuilder[Type]

    override def unary_-(): Type = builder.create(-this.toRadians)

    def reduce: Type

    def +(that: Angle): Type = builder.create(this.toRadians + that.toRadians)

    def -(that: Angle): Type = builder.create(this.toRadians - that.toRadians)

    override def *(that: Real): Type = builder.create(this.toRadians * that)

    override def /(that: Real): Type = builder.create(this.toRadians / that)

    override def ?+(that: Real) = that match {
        case angle: Angle => Some(Radians(this.toRadians + angle.toRadians))
        case _ => super.?+(that)
    }

    override def ?*(that: Real) = that match {
        case angle: Angle => Some(Radians(toRadians * angle.toRadians))
        case _ => Some(Radians(toRadians * that))
    }

}

object Angle extends Enumeration {

    trait AngleBuilder[T] {

        def create(radians: Real): T

    }

    implicit class AngularInt(val int: Int)
            extends Angular(int)

    implicit class Angular(val value: Real) {

        def radians: Radians = Radians(value)

        def radian = radians

        def rad = radians

        def degrees: Degrees = new Degrees(value)

        def degree = degrees

        def ° = degrees

        def gradians: Grads = new Grads(value)

        def grad = gradians

        def gon = gradians

        def turns: Turns = new Turns(value)

        def turn = turns

    }

}

object Classification extends Enumeration {

    type Classification = Value

    final val Acute, Right, Obtuse, Straight, Reflex, Full = Classification

}

/**
 * 2 pi radians, or 360 degrees.
 */
object FullAngle
        extends KnownRadians(2 * Pi) {

    override def classify = Classification.Full

}

/**
 * pi radians, or 180 degrees.
 */
object HalfAngle
        extends KnownRadians(Pi) {

    override def classify = Classification.Straight

}

/**
 * pi/2 radians, or 90 degrees.
 */
object RightAngle
        extends KnownRadians(Pi / 2) {

    override def classify = Classification.Right

}

object EmptyAngle
        extends KnownRadians(0)
        with Empty {

    override def unary_-() = this

    override def isEmpty = super[Empty].isEmpty

    override def classify = Classification.Acute

    override def variables = super[Empty].variables

    override def toConstant = Some(this)

    override def reduce = this

}

trait Radians
        extends AnyRef
        with Angle {

    type Type = Radians

    def value: Real

    def toRadians = value

    def reduce = Radians(Modulo(value, 2 * Pi).remainder)

    def +(that: Radians): Radians = Radians(this.value + that.value)

    def -(that: Radians): Radians = Radians(this.value - that.value)

    override def unary_-(): Radians = Radians(-value)

    override def toString = value.toString

    implicit def builder = Radians.Builder

}

class KnownRadians protected[angular](val value: Real)
        extends Radians

object Radians {

    implicit def apply(re: Real): Radians = re match {
        case Zero => EmptyAngle
        case rad: Radians => rad
        case angle: Angle => new KnownRadians(angle.toRadians)
        case _ => new KnownRadians(re)
    }

    implicit object Builder extends AngleBuilder[Radians] {

        def create(radians: Real) = Radians(radians)

    }

}

class Degrees(val value: Real)
        extends AnyRef
        with Angle {

    type Type = Degrees

    def toRadians = value * Pi / 180

    def +(that: Degrees): Degrees = new Degrees(this.value + that.value)

    def -(that: Degrees): Degrees = new Degrees(this.value - that.value)

    override def unary_-(): Degrees = Degrees(-value)

    override def toString = value.toString + "°"

    implicit def builder = Degrees.Builder

    def reduce = Degrees(Modulo(value, 360).remainder)

}

object Degrees {

    def apply(re: Real): Degrees = re match {
        case deg: Degrees => deg
        case _ => new Degrees(re * 180 / Pi)
    }

    implicit object Builder
            extends AngleBuilder[Degrees] {

        def create(radians: Real) = apply(radians)

    }

}

class Grads(val value: Real)
        extends AnyRef
        with Angle {

    type Type = Grads

    def toRadians = value * Pi / 200

    override def unary_-(): Grads = new Grads(-value)

    override def toString = value.toString + " grad"

    implicit def builder = ??? //TODO

    def reduce = ??? //TODO

}

class Turns(val value: Real)
        extends AnyRef
        with Angle {

    type Type = Turns

    def toRadians = value * 2 * Pi

    override def unary_-(): Turns = new Turns(-value)

    override def toString = value.toString + " turns"

    implicit def builder = ???

    def reduce = ??? //TODO

}
