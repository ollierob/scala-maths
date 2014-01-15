package net.ollie.maths.numbers

import scala.Some
import scala.math.BigDecimal.RoundingMode
import scala.math.BigDecimal.RoundingMode._

/**
 * Integer classes.
 * Created by Ollie on 01/01/14.
 */
trait IntegerNumber
        extends RationalNumber {

    final def numerator = this

    final def denominator = One

    protected override def eval(precision: Precision)(implicit mode: RoundingMode.RoundingMode): BigDecimal = precision(BigDecimal(evaluate))(mode)

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode) = BigDecimal(evaluate)

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

    override def isEmpty = evaluate == 0

    override def equals(that: RealNumber) = that match {
        case int: IntegerNumber => this.equals(int)
        case _ => super.equals(that)
    }

    def equals(that: IntegerNumber): Boolean = this.eq(that) || this.evaluate == that.evaluate

    override def toString = evaluate.toString

    override def hashCode = evaluate.hashCode

}

object IntegerNumber {

    implicit def apply(int: Int): IntegerNumber = int match {
        case 0 => Zero
        case 1 => One
        case _ if int > 0 => NaturalNumber(int)
        case _ => new ExactInteger(int)
    }

    implicit def apply(int: BigInt): IntegerNumber = if (int == 0) Zero else new ExactInteger(int)

    def negate(i: IntegerNumber): IntegerNumber = new NegatedInteger(i)

    def divide(numerator: IntegerNumber, denominator: IntegerNumber): RealNumber = IntegerFraction(numerator, denominator)

    def abs(i: IntegerNumber): NaturalNumber = NaturalNumber(i.evaluate.abs)

    def pow(n: NaturalNumber): IntegerNumber = ???

    def is(n: Number): Option[IntegerNumber] = n match {
        case i: IntegerNumber => Some(i)
        case _ => None
    }

}

class ExactInteger(val evaluate: BigInt)
        extends AnyRef
        with IntegerNumber

object NegatedInteger {

    def unapply(negated: NegatedInteger): Option[IntegerNumber] = Some(negated.i)

}

class NegatedInteger(val i: IntegerNumber)
        extends NegatedRealNumber(i)
        with IntegerNumber {

    override def unary_-() = i

    def evaluate = -(i.evaluate)

    override def isEmpty = super[NegatedRealNumber].isEmpty

    override def equals(that: IntegerNumber) = that match {
        case NegatedInteger(j) => i == j || super.equals(that)
        case _ => super.equals(that)
    }

}
