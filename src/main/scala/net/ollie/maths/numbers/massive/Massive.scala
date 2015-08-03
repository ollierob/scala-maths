package net.ollie.maths.numbers.massive

import net.ollie.maths._
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{One, Unity, Zero}

/**
 * Real finite numbers that (probably) cannot be expressed in decimal form because they are so large.
 *
 * These are handled in their own number system, because most functions will need to evaluate them
 * to be able to evaluate themselves.
 *
 * Created by Ollie on 12/01/14.
 * @see [[Infinitesimal]]
 * @see [[NaturalTetration]]
 */
trait Massive
        extends Constant
        with MaybeReal {

    type System = Massive

    override final def toReal: Option[Real] = Some(closestReal)

    def closestReal: Real = Infinity

    override def inverse: Constant = this.toReal match {
        case Some(re) => re.inverse
        case _ => new InvertedMassive(this)
    }

    override def unary_-(): Massive = Massive.negate(this)

    def abs: PositiveReal = this.toReal match {
        case Some(re) => re.abs
        case _ => Infinity
    }

    def +(that: Massive): Massive = Massive.series(Seq(this, that))

    final def +(that: Real): Massive = this + Massive(that)

    def *(that: Massive): Massive = Massive.product(Seq(this, that))

    final def *(that: Real): Massive = this * Massive(that)

    override def ?+(that: Constant): Option[Constant] = that match {
        case re: Real => Some(this + Massive(re))
        case m: Massive => Some(this + m)
        case _ => None
    }

    override def ?*(that: Constant)(leftToRight: Boolean) = that match {
        case m: Massive => Some(m)
        case _ => None
    }

    override def ?^(that: Constant) = ???

    override def df(x: Variable) = MassiveZero

    override def isEmpty = false

}

object Massive
        extends NumberIdentityArithmetic[Massive] {

    def zero: Massive with EmptyConstant = MassiveZero

    def one: Massive with Unity = MassiveOne

    def apply(): Massive = MassiveZero

    def apply(n: Constant): Option[Massive] = n match {
        case re: Real => Some(Massive(re))
        case m: Massive => Some(m)
        case _ => None
    }

    implicit def apply(re: Real): Massive = if (re.isEmpty) Massive() else new PromotedMassive(re)

    def negate[M <: Massive](m: M): NegatedMassive[M] = new NegatedMassive(m)

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
            extends NumberConversionArithmetic[Real, Massive]
            with AdditionArithmetic[Real, Massive, Massive]
            with MultiplicationArithmetic[Real, Massive, Massive] {

        def zero = Massive.zero

        def one = Massive.one

        def apply(from: Real) = from

        def add(left: Real, right: Massive) = Massive(left) + right

        def multiply(left: Real, right: Massive): Massive = Massive(left) * right

        def convert(n: Constant) = Massive(n)

    }

    implicit object MassiveRealArithmetic
            extends AdditionArithmetic[Massive, Real, Massive]
            with MultiplicationArithmetic[Massive, Real, Massive] {

        def zero = Massive.zero

        def one = Massive.one

        def multiply(left: Massive, right: Real) = left * Massive(right)

        def add(left: Massive, right: Real) = left + Massive(right)

    }

}

object MassiveZero
        extends Massive
        with EmptyConstant {

    override def isEmpty = true

    override def abs = Zero

    override def inverse = Zero.inverse

    override def ?+(that: Constant) = Some(that)

    override def ?*(that: Constant)(leftToRight: Boolean) = Some(this)

    override def variables = super[EmptyConstant].variables

    override def unary_-() = this

    override def toConstant: Option[Massive with EmptyConstant] = Some(this)

    override def df(x: Variable) = this

    override def closestReal = Infinity

    def tryReduce = Some(Zero)

}

class PromotedMassive(val re: Real)
        extends Massive {

    override def unary_-() = -re

    override def inverse = re.inverse

    override def isEmpty = re.isEmpty

    override def closestReal = re

    override def toString = re.toString

    override def hashCode = re.hashCode

}

object MassiveOne
        extends PromotedMassive(One)
        with Unity {

    override def abs = super[Unity].abs

}

class NegatedMassive[M <: Massive](val of: M)
        extends Massive {

    override def unary_-(): M = of

    override def isEmpty = of.isEmpty

    override def closestReal = -of.closestReal

    override def inverse = -(of.inverse)

    override def abs = of.abs

    override def toString = s"-($of)"

}

class InvertedMassive(val of: Massive)
        extends Infinitesimal {

    override def inverse = of

    override def toReal: Option[Real] = ???

}

class MassiveSeries(override val terms: Seq[Massive])
        extends ConstantSeries(terms)
        with Massive {

    override def unary_-() = Massive.series(terms.map(-_))

    override def +(that: Massive) = Massive.series(simplify(that match {
        case s: MassiveSeries => terms ++ s.terms
        case _ => terms :+ that
    }))

    override def closestReal = terms.map(_.closestReal).reduceLeft((l, r) => l + r)

    protected[this] def tryAdd(left: Massive, right: Massive) = None

}

class MassiveProduct(override val terms: Seq[Massive])
        extends ConstantProduct(terms)(Massive)
        with Massive {

    override protected[this] def apply(terms: Seq[Massive]) = Massive.product(terms)

    override def *(that: Massive) = Massive.product(simplify(that match {
        case p: MassiveProduct => terms ++ p.terms
        case _ => terms :+ that
    }))

    override def closestReal = terms.map(_.closestReal).reduceLeft((l, r) => l * r)

    protected[this] def tryMultiply(left: Massive, right: Massive) = None

}