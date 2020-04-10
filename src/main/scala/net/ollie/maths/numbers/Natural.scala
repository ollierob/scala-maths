package net.ollie.maths.numbers

import net.ollie.maths.numbers.Natural.FactorialCache
import net.ollie.maths.numbers.constants.{One, Three, Two, Zero}
import net.ollie.maths.{NotEvaluable, Operation}
import net.ollie.utils.ValueCache

/**
 * Positive integer classes, including zero.
 * Created by Ollie on 02/01/14.
 */
trait Natural
    extends Integer with PositiveReal {

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

    final def *(i: Int): Integer = this * Integer(i) //Helps resolve ambiguous reference errors

    def /(that: Natural): PositiveReal with Rational = Natural.divide(this, that)

    override def ^(that: Integer) = that match {
        case n: Natural => this.^(n)
        case _ => super.^(that)
    }

    def ^(that: Natural): Natural = Natural.power(this, that)

    def !(implicit cache: FactorialCache): Natural = Natural.factorial(this)(cache)

}

object Natural {

    private val ZERO = BigInt(0)
    private val ONE = BigInt(1)

    implicit def apply(int: Int): Natural = int match {
        case 0 => Zero
        case 1 => One
        case 2 => Two
        case 3 => Three
        case _ => new ExactNatural(int)
    }

    implicit def convert(int: Integer): Natural = apply(int).right.getOrElse(Operation.illegal(s"Int $int is negative!"))

    def apply(int: BigInt): Natural = int match {
        case ZERO => Zero
        case ONE => One
        case _ => new ExactBigNatural(int)
    }

    def apply(int: Integer): Either[Integer, Natural] = int match {
        case Zero => Right(Zero)
        case n: Natural => Right(n)
        case _ if int.isStrictlyPositive => Right(int.abs)
        case _ => Left(int)
    }

    def unapply(n: Natural): Option[Int] = n.toInt

    def divide(numerator: Natural, denominator: Natural): PositiveReal with Rational = IntegerFraction.common(numerator, denominator) match {
        case Some(m) => m.abs
        case _ => IntegerFraction.reduce(numerator, denominator) match {
            case Some((i1, i2)) => divide(i1.abs, i2.abs)
            case _ => new NaturalFraction(numerator, denominator)
        }
    }

    def inverse(n: Natural): PositiveReal with Rational = divide(One, n)

    def power(base: Natural, power: Natural): Natural = new NaturalPower(base, power)

    def factorial(n: Natural)(implicit cache: FactorialCache): Natural = cache(n)

    def computeFactorial(n: Natural, cache: FactorialCache): Natural = n match {
        case Zero => One
        case One => One
        case _ => new Factorial(n)(cache)
    }

    trait FactorialCache extends Function[Natural, Natural]

    implicit object FactorialCache
        extends ValueCache[Natural, Natural]
            with FactorialCache {

        override protected def compute(n: Natural): Natural = computeFactorial(n, FactorialCache.this)

    }

}

class ExactNatural(override val int: Int)
    extends ExactInteger(int)
        with Natural {

    require(int >= 0, "Int [" + int + "] is not positive!")

}

class ExactBigNatural(override val evaluate: BigInt)
    extends ExactBigInteger(evaluate)
        with Natural {

    require(evaluate >= 0, "BigInt [" + evaluate + "] is not positive!")

}

class NaturalFraction(override val numerator: Natural, override val denominator: Natural)
    extends IntegerFraction(numerator, denominator)
        with PositiveReal {

    override def abs = this

    override def inverse = Natural.divide(denominator, numerator)

}

class NaturalPower(override val base: Natural, override val power: Natural)
    extends PrincipalRealToIntegerPower(base, power) with Natural {

    override def isEven = base.isEven || power.isEven

    override def isEmpty = super[PrincipalRealToIntegerPower].isEmpty

    def evaluate = power.toInt match {
        case Some(i: Int) => base.evaluate.pow(i)
        case _ => ??? //TODO
    }

}

private class Factorial(val n: Natural)(implicit cache: FactorialCache)
    extends Natural {

    private lazy val evaluated = n.evaluate * (n.decr ! (cache)).evaluate

    def evaluate = evaluated

    override def isEven = n > 1

    override def toString = s"$n!"

}

object NaturalInfinity
    extends Natural
        with RealInfinity
        with NotEvaluable {

    override def abs = this

    override def isEmpty = false

    override def evaluate(precision: Precision) = Operation.overflow(s"Cannot evaluate $this")

    override def evaluate = Operation.overflow(s"Cannot evaluate $this")

    override def inverse = Zero

}