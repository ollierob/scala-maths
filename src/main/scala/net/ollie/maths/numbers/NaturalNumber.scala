package net.ollie.maths.numbers

import scala.collection.mutable

import net.ollie.maths.{Operation, IdentityArithmetic, Number}
import net.ollie.maths.numbers.real.RealToIntegerPower

/**
 * Positive integer classes.
 * Created by Ollie on 02/01/14.
 */
trait NaturalNumber
        extends IntegerNumber
        with PositiveRealNumber {

    override def abs: NaturalNumber = this

    final override def +(that: IntegerNumber): IntegerNumber = that match {
        case n: NaturalNumber => this + n
        case _ => super.+(that)
    }

    def +(that: NaturalNumber): NaturalNumber = NaturalNumber(this.evaluate + that.evaluate)

    def +(that: Int): NaturalNumber = this + NaturalNumber(that) //Required to clarify implicit lookup

    override def inverse = NaturalNumber.inverse(this)

    def succ: NaturalNumber = this + One

    def decr: NaturalNumber = NaturalNumber(this.evaluate - 1)

    final override def *(that: IntegerNumber): IntegerNumber = that match {
        case n: NaturalNumber => this * n
        case _ => super.*(that)
    }

    def *(that: NaturalNumber): NaturalNumber = NaturalNumber(this.evaluate * that.evaluate)

    def /(that: NaturalNumber): PositiveRealNumber = NaturalNumber.divide(this, that)

    def ^(that: NaturalNumber): NaturalNumber = NaturalNumber.power(this, that)

    def ! : NaturalNumber = NaturalNumber.factorial(this)

}

object NaturalNumber {

    implicit def apply(int: Int): NaturalNumber = int match {
        case 0 => Zero
        case 1 => One
        case _ => new ExactNaturalNumber(int)
    }

    implicit def convert(int: IntegerNumber): NaturalNumber = apply(int).right.getOrElse(Operation.illegal(s"Int $int is negative!"))

    def apply(int: BigInt): NaturalNumber = if (int == 0) Zero else new ExactBigNaturalNumber(int)

    def apply(int: IntegerNumber): Either[IntegerNumber, NaturalNumber] = int match {
        case Zero => Right(Zero)
        case n: NaturalNumber => Right(n)
        case _ if int.isStrictlyPositive => Right(NaturalNumber(int.evaluate))
        case _ => Left(int)
    }

    def divide(numerator: NaturalNumber, denominator: NaturalNumber): PositiveRealNumber = NaturalNumberFraction(numerator, denominator)

    def inverse(n: NaturalNumber): PositiveRealNumber = divide(One, n)

    def power(base: NaturalNumber, power: NaturalNumber): NaturalNumber = new NaturalNumberPower(base, power)

    private val factorials: mutable.Map[NaturalNumber, NaturalNumber] = new mutable.HashMap()

    def factorial(n: NaturalNumber) = n match {
        case Zero => One
        case One => One
        case _ => factorials.get(n) match {
            case Some(m) => m
            case _ => {
                val f = new Factorial(n)
                factorials.put(n, f)
                f
            }
        }
    }

    implicit object IntegerToNaturalNumber
            extends IdentityArithmetic[IntegerNumber, NaturalNumber] {

        def convert(i: IntegerNumber) = NaturalNumber(i) match {
            case r if r.isRight => Some(r.right.get)
            case _ => None
        }
    }

}

object One
        extends ExactNaturalNumber(1) {

    override def ! = this

    override def unary_-() = MinusOne

    override def decr = Zero

    override def ?*(that: Number): Option[Number] = Some(that)

    override def *(that: NaturalNumber) = that

    override def ?*(that: RealNumber) = Some(that)

}

object MinusOne
        extends ExactInteger(-1) {

    override def unary_-() = One

    override def ^(that: IntegerNumber) = if (that.isEven) One else this

}

class ExactNaturalNumber(int: Int)
        extends ExactInteger(int)
        with NaturalNumber {

    require(int >= 0)

}

class ExactBigNaturalNumber(int: BigInt)
        extends ExactBigInteger(int)
        with NaturalNumber {

    require(int >= 0)

}

class Factorial(val n: NaturalNumber)
        extends NaturalNumber {

    private lazy val evaluated = n.evaluate * (n.decr !).evaluate

    def evaluate = evaluated

    override def toString = n.toString + "!"

}

object NaturalNumberFraction {

    def apply(n1: NaturalNumber, n2: NaturalNumber): PositiveRealNumber = IntegerFraction.common(n1, n2) match {
        case Some(m) => m.abs
        case _ => IntegerFraction.reduce(n1, n2) match {
            case Some((i1, i2)) => apply(i1.abs, i2.abs)
            case _ => new NaturalNumberFraction(n1, n2)
        }
    }

}

class NaturalNumberFraction(override val numerator: NaturalNumber, override val denominator: NaturalNumber)
        extends IntegerFraction(numerator, denominator)
        with PositiveRealNumber {

    override def abs = this

    override def inverse = NaturalNumberFraction(denominator, numerator)

}

class NaturalNumberPower(override val base: NaturalNumber, override val power: NaturalNumber)
        extends RealToIntegerPower(base, power)
        with NaturalNumber {

    def evaluate = power.toInt match {
        case Some(i: Int) => base.evaluate.pow(i)
        case _ => ??? //TODO
    }

}