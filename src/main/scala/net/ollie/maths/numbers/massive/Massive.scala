package net.ollie.maths.numbers.massive

import net.ollie.maths._
import net.ollie.maths.numbers._
import scala.Some

/**
 * Real finite numbers that (probably) cannot be expressed in decimal form because they are so large.
 *
 * These are handled in their own number system, because most functions will need to evaluate them
 * to be able to evaluate themselves.
 *
 * Created by Ollie on 12/01/14.
 * @see [[Infinitesimal]]
 * @see [[PowerTower]]
 */
trait Massive
        extends Number {

    type System = Massive

    def inverse: Real = this.tryReduce match {
        case Some(re) => re.inverse
        case _ => new InvertedMassive(this)
    }

    def unary_-(): Massive = new NegatedMassive(this)

    def tryReduce: Option[Real]

    def abs: PositiveReal = this.tryReduce match {
        case Some(re) => re.abs
        case _ => Infinity
    }

    def +(that: Massive): Massive = Massive.series(Seq(this, that))

    final def +(that: Real): Massive = this + Massive(that)

    def *(that: Massive): Massive = Massive.product(Seq(this, that))

    final def *(that: Real): Massive = this * Massive(that)

    def ?+(that: Number): Option[Number] = that match {
        case re: Real => Some(this + Massive(re))
        case m: Massive => Some(this + m)
        case _ => None
    }

    def ?*(that: Number)(leftToRight: Boolean) = that match {
        case m: Massive => Some(m)
        case _ => None
    }

    def ?^(that: Number) = ???

    override def df(x: Variable) = MassiveZero

}

object Massive {

    def apply(): Massive = MassiveZero

    def apply(n: Number): Option[Massive] = n match {
        case re: Real => Some(Massive(re))
        case m: Massive => Some(m)
        case _ => None
    }

    implicit def apply(re: Real): Massive = if (re.isEmpty) Massive() else new PromotedMassive(re)

    def series(terms: Seq[Massive]): Massive = terms.filterNot(_.isEmpty) match {
        case Nil => Massive()
        case m :: Nil => m
        case _ => new MassiveSeries(terms)
    }

    def product(terms: Seq[Massive]): Massive = {
        if (terms.find(_.isEmpty).isDefined) Massive()
        terms match {
            case Nil => Massive()
            case m :: Nil => m
            case _ => ???
        }
    }

    implicit object RealMassiveArithmetic
            extends IdentityArithmetic[Real, Massive]
            with AdditionArithmetic[Real, Massive, Massive]
            with MultiplicationArithmetic[Real, Massive, Massive] {

        def zero = Zero

        def one = One

        def promote(from: Real) = from

        def add(left: Real, right: Massive) = Massive(left) + right

        def multiply(left: Real, right: Massive): Massive = Massive(left) * right

        def convert(n: Number) = Massive(n)

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
        with EmptyNumber {

    override def isEmpty = true

    override def abs = Zero

    override def inverse = Zero.inverse

    override def ?+(that: Number) = Some(that)

    override def ?*(that: Number)(leftToRight: Boolean) = Some(this)

    override def variables = super[EmptyNumber].variables

    override def unary_-() = this

    override def toConstant: Option[Massive] = Some(this)

    override def df(x: Variable) = this

    def tryReduce = Some(Zero)

}

class PromotedMassive(val re: Real)
        extends Massive {

    override def unary_-() = -re

    override def inverse = re.inverse

    def isEmpty = re.isEmpty

    def tryReduce = Some(re)

    override def toString = re.toString

    override def hashCode = re.hashCode

}

class NegatedMassive(val of: Massive)
        extends Massive {

    override def unary_-() = of

    def isEmpty = of.isEmpty

    def tryReduce = of.tryReduce match {
        case Some(re) => Some(-re)
        case _ => None
    }

    override def inverse = -(of.inverse)

    override def abs = of.abs

}

class InvertedMassive(val of: Massive)
        extends Infinitesimal {

    //TODO inverse should return massive
    //TODO inverse shouldn't assume positive

}

class MassiveSeries(override val terms: Seq[Massive])
        extends NumberSeries(terms)
        with Massive {

    override def unary_-() = Massive.series(terms.map(-_))

    override def +(that: Massive) = Massive.series(simplify(that match {
        case s: MassiveSeries => terms ++ s.terms
        case _ => terms :+ that
    }))

    def tryReduce: Option[Real] = terms.map(_.tryReduce) match {
        case e if e.contains(None) => None
        case otherwise => Some(otherwise.map(_.get).sum)
    }

    protected[this] def tryAdd(left: Massive, right: Massive) = None

}

class MassiveProduct(override val terms: Seq[Massive])
        extends NumberProduct(terms)
        with Massive {

    override def *(that: Massive) = Massive.product(simplify(that match {
        case p: MassiveProduct => terms ++ p.terms
        case _ => terms :+ that
    }))

    def tryReduce: Option[Real] = terms.map(_.tryReduce) match {
        case e if e.contains(None) => None
        case otherwise => Some(otherwise.map(_.get).product)
    }

    protected[this] def tryMultiply(left: Massive, right: Massive) = None

}