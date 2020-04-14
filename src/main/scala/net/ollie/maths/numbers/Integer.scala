package net.ollie.maths.numbers

import net.ollie.maths.functions.numeric.{Nearest, Roundable}
import net.ollie.maths.numbers.constants.{MinusOne, One, Zero}
import net.ollie.utils.Is

/**
 * Integer classes.
 * Created by Ollie on 01/01/14.
 */
trait Integer
    extends Rational {

    final def numerator = this

    final def denominator = One

    override def evaluate(precision: Precision): BigDecimal = precision(BigDecimal(evaluate))

    override def approximatelyEvaluate(precision: Precision) = evaluate(precision)

    def evaluate: BigInt

    override def unary_-(): Integer = Integer.negate(this)

    override def inverse: Rational = Integer.divide(One, this)

    override def abs: Natural = Integer.abs(this)

    override def squared: Natural = Natural(this.evaluate.pow(2))

    def toInt: Option[Int] = this.evaluate match {
        case i if i.isValidInt => Some(i.toInt)
        case _ => None
    }

    def requireInt: Int = toInt.get

    def isValidInt: Boolean = evaluate.isValidInt

    def +(that: Integer): Integer = Integer(this.evaluate + that.evaluate)

    override def ?+(that: Real) = that match {
        case int: Integer => Some(this + int)
        case _ => super.?+(that)
    }

    def -(that: Integer): Integer = this + (-that)

    def *(that: Integer): Integer = Integer(this.evaluate * that.evaluate)

    override def ?*(that: Real) = {
        that match {
            case int: Integer => Some(this * int)
            case _ => super.?*(that)
        }
    }

    def /(that: Integer): Rational = Integer.divide(this, that)

    def isEven: Boolean = this.evaluate % 2 == 0

    def isOdd: Boolean = !this.isEven

    override def isEmpty = evaluate == 0

    override def equals(that: Real): Boolean = that match {
        case int: Integer => this.equals(int)
        case _ => super.equals(that)
    }

    def equals(that: Integer): Boolean = this.eq(that) || this.evaluate == that.evaluate

    override def compare(that: Real) = that match {
        case i: Integer => evaluate compareTo i.evaluate
        case _ => super.compare(that)
    }

    override def toString = evaluate.toString

    override def hashCode = evaluate.hashCode

}

object Integer
    extends Is[Real] {

    private val ZERO = BigInt(0)
    private val MINUS_ONE = BigInt(-1)

    implicit def apply(int: Int): Integer = apply(int.toLong)

    implicit def apply(int: Long): Integer = int match {
        case -1 => MinusOne
        case _ if int >= 0 => Natural(int)
        case _ => new ExactInteger(int)
    }

    implicit def apply(int: BigInt): Integer = {
        if (int > 0) Natural(int)
        else if (int == ZERO) Zero
        else if (int == MINUS_ONE) MinusOne
        else new ExactBigInteger(int)
    }

    def negate(i: Integer): Integer = {
        if (i.isEmpty) i
        else new NegatedInteger(i)
    }

    def divide(numerator: Integer, denominator: Integer): Rational = IntegerFraction(numerator, denominator)

    def abs(i: Integer): Natural = Natural(i.evaluate.abs)

    override def is(r: Real): Boolean = r.isInstanceOf[Integer]

    def round(r: Real): Integer = r match {
        case Zero => Zero
        case i: Integer => i
        case r: Roundable => r.round
        case _ => Nearest(r)
    }

    def round(bd: BigDecimal): Integer = bd match {
        case Zero.BIG_DECIMAL => Zero
        case _ => apply(bd.rounded.toLong)
    }

    implicit object IntegerArithmetic
        extends Numeric[Integer] {

        def plus(x: Integer, y: Integer) = x + y

        def minus(x: Integer, y: Integer) = x - y

        def times(x: Integer, y: Integer) = x * y

        def negate(x: Integer): Integer = -x

        def fromInt(x: Int): Integer = Integer(x)

        def toInt(x: Integer) = x.evaluate.toInt

        def toLong(x: Integer): Long = x.evaluate.toLong

        def toFloat(x: Integer): Float = x.evaluate.toFloat

        def toDouble(x: Integer): Double = x.evaluate.toDouble

        def compare(x: Integer, y: Integer) = x compare y

        override def parseString(str: String): Option[Integer] = Option.apply(str.toInt)

    }

}

class ExactInteger(val int: Long)
    extends Integer {

    val evaluate = BigInt(int)

    override def ?+(that: Real) = that match {
        case exact: ExactBigDecimal => Some(Real(BigDecimal(evaluate) + exact.of))
        case _ => super.?+(that)
    }

    override def unary_-(): Integer = Integer(-int)

    override def isEven = int % 2 == 0

    override def isNegative = int < 0

    override def isPositive = int > 0;

    override def evaluate(precision: Precision) = precision(int)

    override def toString = int.toString

}

class ExactBigInteger(val evaluate: BigInt)
    extends Integer

object NegatedInteger {

}

class NegatedInteger(val i: Integer)
    extends NegatedReal(i)
        with Integer {

    override def unary_-() = i

    def evaluate = -(i.evaluate)

    override def isEmpty = super[NegatedReal].isEmpty

    override def equals(that: Integer) = that match {
        case j: NegatedInteger => i == -j.i || super.equals(that)
        case _ => super.equals(that)
    }

}
