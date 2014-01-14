package net.ollie.maths.numbers

import scala.collection.mutable
import net.ollie.maths.methods.ApproximatelyEvaluated

/**
 * Positive integer classes.
 * Created by Ollie on 02/01/14.
 */
trait NaturalNumber
        extends IntegerNumber
        with PositiveRealNumber {

    override def abs: NaturalNumber = this

    def +(that: NaturalNumber): NaturalNumber = NaturalNumber(this.evaluate + that.evaluate)

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

    def divide(numerator: NaturalNumber, denominator: NaturalNumber) = new PositiveIntegerFraction(numerator, denominator)

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

class PositiveIntegerFraction(val numerator: NaturalNumber, val denominator: NaturalNumber)
        extends RationalNumber
        with PositiveRealNumber
        with ApproximatelyEvaluated {

    def reduce: RationalNumber = ???

}

class Factorial(val n: NaturalNumber)
        extends NaturalNumber {

    private lazy val evaluated = n.evaluate * (n.decr !).evaluate

    def evaluate = evaluated

    override def toString = n.toString + "!"

}
