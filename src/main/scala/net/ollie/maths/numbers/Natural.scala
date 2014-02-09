package net.ollie.maths.numbers

import scala.collection.mutable

import net.ollie.maths.{Number, Operation}

/**
 * Positive integer classes.
 * Created by Ollie on 02/01/14.
 */
trait Natural
        extends Integer
        with PositiveReal {

    override def abs: Natural = this

    final override def +(that: Integer): Integer = that match {
        case n: Natural => this + n
        case _ => super.+(that)
    }

    def +(that: Natural): Natural = Natural(this.evaluate + that.evaluate)

    def +(that: Int): Natural = this + Natural(that) //Required to clarify implicit lookup

    override def inverse = Natural.inverse(this)

    def succ: Natural = this + One

    def decr: Natural = Natural(this.evaluate - 1)

    final override def *(that: Integer): Integer = that match {
        case n: Natural => this * n
        case _ => super.*(that)
    }

    def *(that: Natural): Natural = Natural(this.evaluate * that.evaluate)

    def *(i: Int): Integer = this * Integer(i) //Helps resolve ambiguous reference errors

    def /(that: Natural): PositiveReal with Rational = Natural.divide(this, that)

    def ^(that: Natural): Natural = Natural.power(this, that)

    def ! : Natural = Natural.factorial(this)

}

object Natural {

    private val ZERO = BigInt(0)
    private val ONE = BigInt(1)

    implicit def apply(int: Int): Natural = int match {
        case 0 => Zero
        case 1 => One
        case _ => new ExactNatural(int)
    }

    implicit def convert(int: Integer): Natural = apply(int).right.getOrElse(Operation.illegal(s"Int $int is negative!"))

    def apply(int: BigInt): Natural = if (int == ZERO) Zero else if (int == ONE) One else new ExactBigNatural(int);

    def apply(int: Integer): Either[Integer, Natural] = int match {
        case Zero => Right(Zero)
        case n: Natural => Right(n)
        case _ if int.isStrictlyPositive => Right(int.abs)
        case _ => Left(int)
    }

    def divide(numerator: Natural, denominator: Natural): PositiveReal with Rational = IntegerFraction.common(numerator, denominator) match {
        case Some(m) => m.abs
        case _ => IntegerFraction.reduce(numerator, denominator) match {
            case Some((i1, i2)) => divide(i1.abs, i2.abs)
            case _ => new NaturalFraction(numerator, denominator)
        }
    }

    def inverse(n: Natural): PositiveReal with Rational = divide(One, n)

    def power(base: Natural, power: Natural): Natural = new NaturalPower(base, power)

    private val factorials: mutable.Map[Natural, Natural] = new mutable.HashMap()

    def factorial(n: Natural) = n match {
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

}

object One
        extends ExactNatural(1) {

    override def ! = this

    override def unary_-() = MinusOne

    override def decr = Zero

    override def ?*(that: Number)(leftToRight: Boolean): Option[Number] = Some(that)

    override def *(that: Natural) = that

    override def ?*(that: Real) = Some(that)

}

object MinusOne
        extends ExactInteger(-1) {

    override def unary_-() = One

    override def ^(that: Integer): Integer = if (that.isEven) One else this

    override def equals(re: Real) = (this eq re) || super.equals(re)

}

class ExactNatural(int: Int)
        extends ExactInteger(int)
        with Natural {

    require(int >= 0)

}

class ExactBigNatural(int: BigInt)
        extends ExactBigInteger(int)
        with Natural {

    require(int >= 0)

}

class Factorial(val n: Natural)
        extends Natural {

    private lazy val evaluated = n.evaluate * (n.decr !).evaluate

    def evaluate = evaluated

    override def isEven = n > 1

    override def toString = n.toString + "!"

}

class NaturalFraction(override val numerator: Natural, override val denominator: Natural)
        extends IntegerFraction(numerator, denominator)
        with PositiveReal {

    override def abs = this

    override def inverse = Natural.divide(denominator, numerator)

}

class NaturalPower(override val base: Natural, override val power: Natural)
        extends RealToIntegerPower(base, power)
        with Natural {

    override def isEven = base.isEven || power.isEven

    def evaluate = power.toInt match {
        case Some(i: Int) => base.evaluate.pow(i)
        case _ => ??? //TODO
    }

}