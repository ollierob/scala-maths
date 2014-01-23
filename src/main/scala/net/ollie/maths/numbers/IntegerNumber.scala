package net.ollie.maths.numbers

import scala.Some

/**
 * Integer classes.
 * Created by Ollie on 01/01/14.
 */
trait IntegerNumber
        extends RationalNumber {

    final def numerator = this

    final def denominator = One

    protected override def eval(precision: Precision): BigDecimal = precision(BigDecimal(evaluate))

    override def approximatelyEvaluate(precision: Precision) = BigDecimal(evaluate)

    def evaluate: BigInt

    override def unary_-(): IntegerNumber = IntegerNumber.negate(this)

    override def inverse: RealNumber = IntegerNumber.divide(One, this)

    override def abs: NaturalNumber = IntegerNumber.abs(this)

    override def squared: NaturalNumber = NaturalNumber(this.evaluate.pow(2))

    def toInt: Option[Int] = this.evaluate match {
        case i if i.isValidInt => Some(i.toInt)
        case _ => None
    }

    def +(that: IntegerNumber): IntegerNumber = IntegerNumber(this.evaluate + that.evaluate)

    override def ?+(that: RealNumber) = that match {
        case int: IntegerNumber => Some(this + int)
        case _ => super.?+(that)
    }

    def -(that: IntegerNumber): IntegerNumber = this + (-that)

    def *(that: IntegerNumber): IntegerNumber = IntegerNumber(this.evaluate * that.evaluate)

    override def ?*(that: RealNumber) = that match {
        case int: IntegerNumber => Some(this * int)
        case _ => super.?*(that)
    }

    def /(that: IntegerNumber): RealNumber = IntegerNumber.divide(this, that)

    def isEven: Boolean = this.evaluate % 2 == 0

    override def isEmpty = evaluate == 0

    override def equals(that: RealNumber): Boolean = that match {
        case int: IntegerNumber => this.equals(int)
        case _ => super.equals(that)
    }

    def equals(that: IntegerNumber): Boolean = this.eq(that) || this.evaluate == that.evaluate

    override def toString = evaluate.toString

    override def hashCode = evaluate.hashCode

}

object IntegerNumber {

    implicit def apply(int: Int): IntegerNumber = int match {
        case _ if int > 0 => NaturalNumber(int)
        case -1 => MinusOne
        case _ => new ExactInteger(int)
    }

    implicit def apply(int: BigInt): IntegerNumber = int match {
        case _ if int > 0 => NaturalNumber(int)
        case -1 => MinusOne
        case _ => new ExactBigInteger(int)
    }

    def negate(i: IntegerNumber): IntegerNumber = new NegatedInteger(i)

    def divide(numerator: IntegerNumber, denominator: IntegerNumber): RealNumber = IntegerFraction(numerator, denominator)

    def abs(i: IntegerNumber): NaturalNumber = NaturalNumber(i.evaluate.abs)

    def pow(n: NaturalNumber): IntegerNumber = ???

    def is(n: Number): Option[IntegerNumber] = n match {
        case i: IntegerNumber => Some(i)
        case _ => None
    }

}

class ExactInteger(val int: Int)
        extends AnyRef
        with IntegerNumber {

    private lazy val evaluated: BigInt = int

    def evaluate = evaluated

    override def ?+(that: RealNumber) = that match {
        case exact: ExactRealNumber => Some(RealNumber(BigDecimal(evaluate) + exact.of))
        case _ => super.?+(that)
    }

    override def unary_-(): IntegerNumber = IntegerNumber(-int)

}

class ExactBigInteger(val evaluate: BigInt)
        extends AnyRef
        with IntegerNumber

object NegatedInteger {

    def unapply(negated: NegatedInteger): Option[Int] = negated.evaluate match {
        case i if i.isValidInt => Some(i.toInt)
        case _ => None
    }

}

class NegatedInteger(val i: IntegerNumber)
        extends NegatedRealNumber(i)
        with IntegerNumber {

    override def unary_-() = i

    private lazy val evaluated = -(i.evaluate)

    def evaluate = evaluated

    override def isEmpty = super[NegatedRealNumber].isEmpty

    override def equals(that: IntegerNumber) = that match {
        case NegatedInteger(j) => i == j || super.equals(that)
        case _ => super.equals(that)
    }

}
