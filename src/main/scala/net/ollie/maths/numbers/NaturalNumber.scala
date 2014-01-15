package net.ollie.maths.numbers

import scala.collection.mutable

/**
 * Positive integer classes.
 * Created by Ollie on 02/01/14.
 */
trait NaturalNumber
        extends IntegerNumber
        with PositiveRealNumber {

    override def abs: NaturalNumber = this

    def +(that: NaturalNumber): NaturalNumber = NaturalNumber(this.evaluate + that.evaluate)

    override def inverse = NaturalNumber.inverse(this)

    def succ: NaturalNumber = this + One

    def decr: NaturalNumber = NaturalNumber(this.evaluate - 1)

    def *(that: NaturalNumber): NaturalNumber = NaturalNumber(this.evaluate * that.evaluate)

    def /(that: NaturalNumber): PositiveRealNumber = NaturalNumber.divide(this, that)

    def ! : NaturalNumber = NaturalNumber.factorial(this)

}

object NaturalNumber {

    implicit def apply(int: Int): NaturalNumber = apply(BigInt(int))

    implicit def convert(int: IntegerNumber): NaturalNumber = apply(int).right.get

    def apply(int: BigInt): NaturalNumber = if (int == 0) Zero else new ExactNaturalNumber(int)

    def apply(int: IntegerNumber): Either[IntegerNumber, NaturalNumber] = int.evaluate match {
        case p if p >= 0 => Right(apply(p))
        case _ => Left(int)
    }

    def divide(numerator: NaturalNumber, denominator: NaturalNumber): PositiveRealNumber = NaturalNumberFraction(numerator, denominator)

    def inverse(n: NaturalNumber): PositiveRealNumber = divide(One, n)

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

}

object One
        extends ExactNaturalNumber(1) {

    override def ! = this

    override def decr = Zero

    override def *(that: NaturalNumber) = that

    override def ?*(that: RealNumber) = Some(that)

}

class ExactNaturalNumber(int: BigInt)
        extends ExactInteger(int)
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
