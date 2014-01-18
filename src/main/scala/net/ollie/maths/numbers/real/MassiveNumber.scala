package net.ollie.maths.numbers.real

import scala.Some

import net.ollie.maths._
import net.ollie.maths.numbers.{One, PositiveRealNumber, RealNumber, Zero}

/**
 * Real finite numbers that are unlikely to be expressible in decimal form.
 * Created by Ollie on 12/01/14.
 */
trait MassiveNumber
        extends Number {

    type System = MassiveNumber

    def abs: PositiveRealNumber = ???

    def inverse: MassiveNumber = ???

    override def unary_-(): MassiveNumber = ???

    def tryReduce: Option[RealNumber]

    def +(that: MassiveNumber) = MassiveNumber.series(this, that)

    def *(that: MassiveNumber) = ???

    def ?+(that: Number): Option[Number] = that match {
        case re: RealNumber => Some(this + MassiveNumber(re))
        case m: MassiveNumber => Some(this + m)
        case _ => None
    }

    def ?*(that: Number): Option[Number] = None

    def ?^(that: Number) = ???

    override def df(x: Variable) = MassiveZero

}

object MassiveNumber {

    implicit def apply(re: RealNumber): MassiveNumber = if (re.isEmpty) MassiveZero else new SomeMassive(re)

    def series(left: MassiveNumber, right: MassiveNumber): MassiveNumber = (left, right) match {
        case (MassiveZero, MassiveZero) => Zero
        case (_, MassiveZero) => left
        case (MassiveZero, _) => right
        case (_, _) => new MassiveSeries(Seq(left, right))
    }

    implicit object RealMassiveArithmetic
            extends IdentityArithmetic[RealNumber, MassiveNumber]
            with AdditionArithmetic[RealNumber, MassiveNumber, MassiveNumber]
            with MultiplicationArithmetic[RealNumber, MassiveNumber, MassiveNumber] {

        def zero = Zero

        def one = One

        def convert(from: RealNumber) = Some(from)

        def add(left: RealNumber, right: MassiveNumber) = MassiveNumber(left) + right

        def multiply(left: RealNumber, right: MassiveNumber): MassiveNumber = MassiveNumber(left) * right

    }

    implicit object MassiveRealArithmetic
            extends AdditionArithmetic[MassiveNumber, RealNumber, MassiveNumber]
            with MultiplicationArithmetic[MassiveNumber, RealNumber, MassiveNumber] {

        def zero = Zero

        def one = One

        def multiply(left: MassiveNumber, right: RealNumber) = left * MassiveNumber(right)

        def add(left: MassiveNumber, right: RealNumber) = left + MassiveNumber(right)

    }

}

object MassiveZero
        extends MassiveNumber
        with Empty {

    override def isEmpty = true

    override def inverse = Zero.inverse

    override def ?+(that: Number) = Some(that)

    override def ?*(that: Number) = Some(this)

    override def variables = super[Empty].variables

    override def unary_-() = this

    def tryReduce = Some(Zero)

}

class SomeMassive(val re: RealNumber)
        extends MassiveNumber {

    override def abs = re.abs

    override def inverse = re.inverse

    def isEmpty = re.isEmpty

    def tryReduce = Some(re)

    override def toString = re.toString

    override def hashCode = re.hashCode

}

class MassiveSeries(val terms: Seq[MassiveNumber])
        extends MassiveNumber {

    private final def series = Series(terms)

    def tryReduce: Option[RealNumber] = terms.map(_.tryReduce) match {
        case e if e.contains(None) => None
        case otherwise => Some(otherwise.map(_.get).sum)
    }

    def isEmpty = series.isEmpty

    override def toString = series.toString

    override def hashCode = series.hashCode

}