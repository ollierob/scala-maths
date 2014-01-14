package net.ollie.maths.numbers.real


import net.ollie.maths._
import net.ollie.maths.numbers.{One, PositiveRealNumber, RealNumber, Zero}
import scala.Some

/**
 * Real finite numbers that are unlikely to be expressible in decimal form.
 * Created by Ollie on 12/01/14.
 */
trait MassiveNumber
        extends Number {

    type System = MassiveNumber

    def abs: PositiveRealNumber = ???

    override def unary_-(): MassiveNumber = ???

    def tryReduce: Option[RealNumber]

    def ?+(that: Number): Option[Number] = None

    def ?*(that: Number): Option[Number] = None

    def +(that: MassiveNumber) = MassiveSeries(this, that)

    def *(that: MassiveNumber) = ???

    override def df(x: Variable) = MassiveZero

}

object MassiveNumber {

    implicit def apply(re: RealNumber): MassiveNumber = if (re.isEmpty) MassiveZero else new SomeMassive(re)

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

    def inverse = Zero.inverse

    override def ?+(that: Number) = Some(that)

    override def ?*(that: Number) = Some(this)

    override def variables = super[Empty].variables

    override def unary_-() = this

    def tryReduce = Some(Zero)

}

class SomeMassive(re: RealNumber)
        extends MassiveNumber {

    override def abs: PositiveRealNumber = re.abs

    override def inverse = re.inverse

    def isEmpty = re.isEmpty

    override def toString = re.toString

    override def hashCode = re.hashCode

    def tryReduce = Some(re)

}

object MassiveSeries {

    def apply(left: MassiveNumber, right: MassiveNumber): MassiveNumber = (left, right) match {
        case (MassiveZero, MassiveZero) => Zero
        case (_, MassiveZero) => left
        case (MassiveZero, _) => right
        case (_, _) => new MassiveSeries(Seq(left, right))
    }

}

class MassiveSeries(terms: Seq[MassiveNumber])
        extends MassiveNumber {

    private final def series = Series(terms)

    def inverse: MassiveNumber = ???

    def tryReduce: Option[RealNumber] = ???

    def isEmpty = series.isEmpty

    override def toString = series.toString

    override def hashCode = series.hashCode

}