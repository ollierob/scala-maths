package net.ollie.maths.numbers.real

import scala.Some

import net.ollie.maths._
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.{One, PositiveReal, Real, Zero}

/**
 * Real finite numbers that are unlikely to be expressible in decimal form.
 * Created by Ollie on 12/01/14.
 */
trait Massive
        extends Number {

    type System = Massive

    def abs: PositiveReal = ???

    def inverse: Massive = ???

    override def unary_-(): Massive = ???

    def tryReduce: Option[Real]

    def +(that: Massive) = Massive.series(this, that)

    def *(that: Massive) = ???

    def ?+(that: Number): Option[Number] = that match {
        case re: Real => Some(this + Massive(re))
        case m: Massive => Some(this + m)
        case _ => None
    }

    protected def ?*(that: Number)(leftToRight: Boolean) = that match {
        case m: Massive => Some(m)
        case _ => None
    }

    def ?^(that: Number) = ???

    override def df(x: Variable) = MassiveZero

}

object Massive {

    implicit def apply(re: Real): Massive = if (re.isEmpty) MassiveZero else new SomeMassive(re)

    def series(left: Massive, right: Massive): Massive = (left, right) match {
        case (MassiveZero, MassiveZero) => Zero
        case (_, MassiveZero) => left
        case (MassiveZero, _) => right
        case (_, _) => new MassiveSeries(Seq(left, right))
    }

    implicit object RealMassiveArithmetic
            extends IdentityArithmetic[Real, Massive]
            with AdditionArithmetic[Real, Massive, Massive]
            with MultiplicationArithmetic[Real, Massive, Massive] {

        def zero = Zero

        def one = One

        def convert(from: Real) = from

        def add(left: Real, right: Massive) = Massive(left) + right

        def multiply(left: Real, right: Massive): Massive = Massive(left) * right

    }

    implicit object MassiveRealArithmetic
            extends AdditionArithmetic[Massive, Real, Massive]
            with MultiplicationArithmetic[Massive, Real, Massive] {

        def zero = Zero

        def one = One

        def multiply(left: Massive, right: Real) = left * Massive(right)

        def add(left: Massive, right: Real) = left + Massive(right)

    }

}

object MassiveZero
        extends Massive
        with Empty {

    override def isEmpty = true

    override def inverse = Zero.inverse

    override def ?+(that: Number) = Some(that)

    override def ?*(that: Number)(leftToRight: Boolean) = Some(this)

    override def variables = super[Empty].variables

    override def unary_-() = this

    def tryReduce = Some(Zero)

}

class SomeMassive(val re: Real)
        extends Massive {

    override def abs = re.abs

    override def inverse = re.inverse

    def isEmpty = re.isEmpty

    def tryReduce = Some(re)

    override def toString = re.toString

    override def hashCode = re.hashCode

}

class MassiveSeries(val terms: Seq[Massive])
        extends Massive {

    private final def series = Series(terms)

    def tryReduce: Option[Real] = terms.map(_.tryReduce) match {
        case e if e.contains(None) => None
        case otherwise => Some(otherwise.map(_.get).sum)
    }

    def isEmpty = series.isEmpty

    override def toString = series.toString

    override def hashCode = series.hashCode

}