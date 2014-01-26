package net.ollie.maths.numbers

import scala.Some

/**
 * Integer classes.
 * Created by Ollie on 01/01/14.
 */
trait Integer
        extends Rational {

    final def numerator = this

    final def denominator = One

    protected override def eval(precision: Precision): BigDecimal = precision(BigDecimal(evaluate))

    override def approximatelyEvaluate(precision: Precision) = BigDecimal(evaluate)

    def evaluate: BigInt

    override def unary_-(): Integer = Integer.negate(this)

    override def inverse: Real = Integer.divide(One, this)

    override def abs: Natural = Integer.abs(this)

    override def squared: Natural = Natural(this.evaluate.pow(2))

    def toInt: Option[Int] = this.evaluate match {
        case i if i.isValidInt => Some(i.toInt)
        case _ => None
    }

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

    def /(that: Integer): Real = Integer.divide(this, that)

    def isEven: Boolean = this.evaluate % 2 == 0

    override def isEmpty = evaluate == 0

    override def equals(that: Real): Boolean = that match {
        case int: Integer => this.equals(int)
        case _ => super.equals(that)
    }

    def equals(that: Integer): Boolean = this.eq(that) || this.evaluate == that.evaluate

    override def toString = evaluate.toString

    override def hashCode = evaluate.hashCode

}

object Integer {

    private val MINUS_ONE = BigInt(-1)

    implicit def apply(int: Int): Integer = int match {
        case _ if int > 0 => Natural(int)
        case -1 => MinusOne
        case _ => new ExactInteger(int)
    }

    implicit def apply(int: BigInt): Integer = if (int > 0) Natural(int) else if (int == MINUS_ONE) MinusOne else new ExactBigInteger(int)

    def negate(i: Integer): Integer = new NegatedInteger(i)

    def divide(numerator: Integer, denominator: Integer): Real = IntegerFraction(numerator, denominator)

    def abs(i: Integer): Natural = Natural(i.evaluate.abs)

    def pow(n: Natural): Integer = ???

    def is(n: Number): Option[Integer] = n match {
        case i: Integer => Some(i)
        case _ => None
    }

    implicit object IntegerArithmetic
            extends Numeric[Integer] {

        def plus(x: Integer, y: Integer) = x + y

        def minus(x: Integer, y: Integer) = x - y

        def times(x: Integer, y: Integer) = x * y

        def negate(x: Integer): Integer = -x

        def fromInt(x: Int): Integer = x

        def toInt(x: Integer) = x.toInt.get

        def toLong(x: Integer): Long = x.toInt.get.toLong

        def toFloat(x: Integer): Float = x.toInt.get.toFloat

        def toDouble(x: Integer): Double = x.toInt.get.toDouble

        def compare(x: Integer, y: Integer) = x compare y

    }

}

class ExactInteger(val int: Int)
        extends AnyRef
        with Integer {

    private lazy val evaluated: BigInt = int

    def evaluate = evaluated

    override def ?+(that: Real) = that match {
        case exact: ExactReal => Some(Real(BigDecimal(evaluate) + exact.of))
        case _ => super.?+(that)
    }

    override def unary_-(): Integer = Integer(-int)

}

class ExactBigInteger(val evaluate: BigInt)
        extends AnyRef
        with Integer

object NegatedInteger {

    def unapply(negated: NegatedInteger): Option[Int] = negated.evaluate match {
        case i if i.isValidInt => Some(i.toInt)
        case _ => None
    }

}

class NegatedInteger(val i: Integer)
        extends NegatedReal(i)
        with Integer {

    override def unary_-() = i

    private lazy val evaluated = -(i.evaluate)

    def evaluate = evaluated

    override def isEmpty = super[NegatedReal].isEmpty

    override def equals(that: Integer) = that match {
        case NegatedInteger(j) => i == j || super.equals(that)
        case _ => super.equals(that)
    }

}
