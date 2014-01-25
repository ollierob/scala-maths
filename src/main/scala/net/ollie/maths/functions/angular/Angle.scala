package net.ollie.maths.functions.angular

import net.ollie.maths.Empty
import net.ollie.maths.functions.angular.Angle.AngleBuilder
import net.ollie.maths.numbers.{Precision, RealNumber, Zero}
import net.ollie.maths.numbers.real.Pi

trait Angle
        extends RealNumber {

    def toRadians: RealNumber

    def isEmpty = toRadians.isEmpty

    def classify = {
        val rads: RealNumber = toRadians.abs
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

    protected[this] def eval(precision: Precision) = toRadians.evaluate(precision)

    override def unary_-(): Angle = ???

    implicit def builder: AngleBuilder

    def +(that: Angle): Angle = builder.create(this.toRadians + that.toRadians)

    def -(that: Angle): Angle = builder.create(this.toRadians - that.toRadians)

    override def *(that: RealNumber): Angle = builder.create(this.toRadians * that)

    override def ?+(that: RealNumber) = that match {
        case angle: Angle => Some(Radians(this.toRadians + angle.toRadians))
        case _ => super.?+(that)
    }

    override def ?*(that: RealNumber) = that match {
        case angle: Angle => Some(Radians(toRadians * angle.toRadians))
        case _ => Some(Radians(toRadians * that))
    }

}

object Angle extends Enumeration {

    trait AngleBuilder {

        def create(radians: RealNumber): Angle

    }

    implicit class AngularInt(val int: Int)
            extends Angular(int)

    implicit class Angular(val value: RealNumber) {

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

object FullAngle
        extends Radians(2 * Pi) {

    override def classify = Classification.Full

}

object HalfAngle
        extends Radians(Pi) {

    override def classify = Classification.Straight

}

object RightAngle
        extends Radians(Pi / 2) {

    override def classify = Classification.Right

}

object EmptyAngle
        extends Radians(0)
        with Empty {

    override def unary_-() = this

    override def isEmpty = super[Empty].isEmpty

    override def classify = Classification.Acute

    override def variables = super[Empty].variables

}

class Radians(val value: RealNumber)
        extends AnyRef
        with Angle {

    def toRadians = value

    def +(that: Radians): Radians = Radians(this.value + that.value)

    def -(that: Radians): Radians = Radians(this.value - that.value)

    override def unary_-(): Radians = Radians(-value)

    override def toString = value.toString

    implicit def builder = Radians.Builder
}

object Radians {

    implicit def apply(re: RealNumber): Radians = re match {
        case Zero => EmptyAngle
        case rad: Radians => rad
        case angle: Angle => new Radians(angle.toRadians)
        case _ => new Radians(re)
    }

    implicit object Builder extends AngleBuilder {

        def create(radians: RealNumber) = Radians(radians)

    }

}

class Degrees(val value: RealNumber)
        extends AnyRef
        with Angle {

    def toRadians = value * Pi / 180

    def +(that: Degrees): Degrees = new Degrees(this.value + that.value)

    def -(that: Degrees): Degrees = new Degrees(this.value - that.value)

    override def unary_-(): Degrees = Degrees(-value)

    override def toString = value.toString + "°"

    implicit def builder = Degrees.Builder

}

object Degrees {

    def apply(re: RealNumber): Degrees = re match {
        case deg: Degrees => deg
        case _ => new Degrees(re * 180 / Pi)
    }

    implicit object Builder
            extends AngleBuilder {

        def create(radians: RealNumber) = apply(radians)

    }

}

class Grads(val value: RealNumber) extends AnyRef with Angle {

    def toRadians = value * Pi / 200

    override def unary_-(): Grads = new Grads(-value)

    override def toString = value.toString + " grad"

    implicit def builder = ???

}

class Turns(val value: RealNumber) extends AnyRef with Angle {

    def toRadians = value * 2 * Pi

    override def unary_-(): Turns = new Turns(-value)

    override def toString = value.toString + " turns"

    implicit def builder = ???

}
