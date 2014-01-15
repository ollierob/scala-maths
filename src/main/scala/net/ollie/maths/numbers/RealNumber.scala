package net.ollie.maths.numbers

import scala.Some
import scala.collection.mutable
import scala.math.BigDecimal.RoundingMode.RoundingMode

import net.ollie.maths._
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.real.RealPower

/**
 * Created by Ollie on 01/01/14.
 */
trait RealNumber
        extends Number
        with Evaluable {

    final type System = RealNumber

    override def replace(variables: Map[Variable, Expression]) = this

    override def unary_-(): RealNumber = RealNumber.negate(this)

    override def df(x: Variable): RealNumber with Empty = Zero

    def inverse: RealNumber = RealNumber.inverse(this)

    def abs = RealNumber.abs(this)

    def +(that: RealNumber): RealNumber = this ?+ that match {
        case Some(n) => n
        case _ => that ?+ this match {
            case Some(m) => m
            case _ => RealSeries(this, that)
        }
    }

    def ?+(that: RealNumber): Option[RealNumber] = that match {
        case Zero => Some(this)
        case _ => None
    }

    def ?+?(that: RealNumber): Option[RealNumber] = this ?+ that match {
        case Some(m) => Some(m)
        case _ => that ?+ this match {
            case Some(m) => Some(m)
            case _ => None
        }
    }

    def ?+(that: Number) = that match {
        case re: RealNumber => Some(this + re)
        case _ => None
    }

    def -(that: RealNumber): RealNumber = this + (-that)

    def *(that: RealNumber): RealNumber = this ?* that match {
        case Some(n) => n
        case _ => that ?* this match {
            case Some(m) => m
            case _ => RealProduct(this, that)
        }
    }

    def ?*(that: RealNumber): Option[RealNumber] = that match {
        case Zero => Some(Zero)
        case One => Some(this)
        case _ => None
    }


    /**
     * Shortcut for this ?* that and that ?* this
     * @param that
     * @return
     */
    def ?*?(that: RealNumber): Option[RealNumber] = this ?* that match {
        case Some(m) => Some(m)
        case _ => that ?* this match {
            case Some(m) => Some(m)
            case _ => None
        }
    }

    /**
     *
     * @param that
     * @return
     */
    def ?*(that: Number) = that match {
        case re: RealNumber => Some(this * re)
        case _ => None
    }

    def /(that: RealNumber): RealNumber = this * that.inverse

    def ^(that: IntegerNumber): RealNumber = RealNumber.pow(this, that)

    def squared: PositiveRealNumber = (this ^ 2).abs

    def >(that: RealNumber): Boolean = this.compareTo(that) > 0

    def >=(that: RealNumber): Boolean = this.compareTo(that) >= 0

    def <(that: RealNumber): Boolean = this.compareTo(that) < 0

    def <=(that: RealNumber): Boolean = this.compareTo(that) <= 0

    def compareTo(that: RealNumber): Int = {
        if (this eq that) 0
        else this.tryCompareTo(that) match {
            case Some(i) => i
            case _ => that.tryCompareTo(this) match {
                case Some(j) => j
                case _ => this.evaluate(SinglePrecision).compare(that.evaluate(SinglePrecision))
            }
        }
    }

    def isStrictlyPositive: Boolean = this > 0

    protected def tryCompareTo(that: RealNumber): Option[Int] = None

    override def equals(expression: Number) = expression match {
        case re: RealNumber => this.equals(re)
        case _ => super.equals(expression)
    }

    def equals(that: RealNumber): Boolean = super.equals(that) || (this ?== that match {
        case Some(b) => b
        case _ => that ?== this match {
            case Some(b) => b
            case _ => this.evaluate(SinglePrecision) == that.evaluate(SinglePrecision)
        }
    })

    def ?==(that: RealNumber): Option[Boolean] = None

}

object RealNumber {

    implicit def apply(int: Int): RealNumber = IntegerNumber(int)

    def apply(value: BigDecimal): RealNumber = if (value == 0) Zero else new ExactRealNumber(value)

    def negate(re: RealNumber) = if (re.isEmpty) Zero else new NegatedRealNumber(re)

    def inverse(re: RealNumber) = new InverseRealNumber(re)

    def abs(re: RealNumber): PositiveRealNumber = re match {
        case Zero => Zero
        case p: PositiveRealNumber => p
        case _ => new AbsRealNumber(re)
    }

    def pow(base: RealNumber, power: IntegerNumber): RealNumber = RealPower(base, power)

    implicit object NumberToReal
            extends IdentityArithmetic[Number, RealNumber] {

        def convert(from: Number): Option[RealNumber] = from match {
            case re: RealNumber => Some(re)
            case _ => None
        }

    }

    implicit object RealArithmetic
            extends AdditionArithmetic[RealNumber, RealNumber, RealNumber]
            with MultiplicationArithmetic[RealNumber, RealNumber, RealNumber]
            with scala.math.Numeric[RealNumber] {

        def add(x: RealNumber, y: RealNumber) = plus(x, y)

        def plus(x: RealNumber, y: RealNumber) = x + y

        def minus(x: RealNumber, y: RealNumber) = x - y

        def times(x: RealNumber, y: RealNumber) = ???

        def negate(x: RealNumber) = -x

        def fromInt(x: Int) = RealNumber(x)

        def toInt(x: RealNumber) = x.evaluate(IntegerPrecision).toInt

        def toLong(x: RealNumber) = x.evaluate(IntegerPrecision).toLong

        def toFloat(x: RealNumber) = x.evaluate(SinglePrecision).toFloat

        def toDouble(x: RealNumber) = x.evaluate(DoublePrecision).toDouble

        def compare(x: RealNumber, y: RealNumber) = x.compareTo(y)

        def multiply(left: RealNumber, right: RealNumber) = left * right

    }

}

class ExactRealNumber(val value: BigDecimal)
        extends AnyRef
        with RealNumber
        with ApproximatelyEvaluated {

    override def ?+(expr: RealNumber) = expr match {
        case that: ExactRealNumber => Some(RealNumber(this.value + that.value))
        case _ => None
    }

    def isEmpty = value == 0

    override def toString = value.toString

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode) = precision(value)

}

class NegatedRealNumber(val of: RealNumber)
        extends RealNumber {

    protected[this] def eval(precision: Precision)(implicit mode: RoundingMode) = -(of.evaluate(precision))

    override def variables = super[RealNumber].variables

    override def unary_-() = of

    override def toConstant = Some(this)

    def isEmpty = of.isEmpty

    override def toString = s"-($of)"

    override def hashCode = -of.hashCode

}

class InverseRealNumber(val of: RealNumber)
        extends RealNumber
        with ApproximatelyEvaluated {

    require(!of.isEmpty)

    override def inverse = of

    def isEmpty = false

    override def ?*(that: RealNumber) = that match {
        case real: RealNumber if of == real => Some(One)
        case _ => super.?*(that)
    }

    override def toString = "1/" + of.toString

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode) = 1 / of.evaluate(precision)

}

class AbsRealNumber(val re: RealNumber)
        extends PositiveRealNumber {

    protected[this] def eval(precision: Precision)(implicit mode: RoundingMode) = re.evaluate(precision).abs

    def isEmpty = re.isEmpty

    override def toString = "|" + re.toString + "|"

}

object RealSeries {

    def apply(left: RealNumber, right: RealNumber): RealNumber = (left, right) match {
        case (Zero, _) => right
        case (_, Zero) => left
        case _ => new RealSeries(Seq(left, right))
    }

    def apply(terms: Seq[RealNumber]): RealNumber = terms.filterNot(_.isEmpty) match {
        case Nil => Zero
        case term :: Nil => term
        case _ => new RealSeries(terms)
    }

}

class RealSeries private(val terms: Seq[RealNumber])
        extends RealNumber
        with ApproximatelyEvaluated {

    private final def series = Series(terms)

    override def toConstant = super[RealNumber].toConstant

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode) = {
        terms.map(_.approximatelyEvaluate(precision)).sum
    }

    override def ?+(that: RealNumber) = Some(RealSeries(terms :+ that)) //TODO simplify

    def isEmpty = series.isEmpty

    override def toString = series.toString

}

object RealProduct {

    def apply(left: RealNumber, right: RealNumber): RealNumber = if (left.isEmpty || right.isEmpty) Zero else new RealProduct(Seq(left, right))

    def apply(terms: Seq[RealNumber]): RealNumber = terms match {
        case Nil => Zero
        case item :: Nil => item
        case _ if terms.contains(None) => Zero
        case _ => new RealProduct(terms)
    }

}

class RealProduct private(val terms: Seq[RealNumber])
        extends RealNumber
        with ApproximatelyEvaluated {

    private final def product = Product(terms)

    override def approximatelyEvaluate(precision: Precision)(implicit mode: RoundingMode) = {
        terms.map(_.approximatelyEvaluate(precision)).product
    }

    override def ?*(that: RealNumber) = {
        if (that.isEmpty) Some(Zero)
        else Some(RealProduct(that match {
            case product: RealProduct => simplify(product.terms, this.terms)
            case _ => simplify(that, this.terms)
        }))
    }

    private def simplify(left: Seq[RealNumber], right: Seq[RealNumber]): Seq[RealNumber] = {
        left.foldLeft(right)((seq, next) => simplify(next, seq))
    }

    private def simplify(term: RealNumber, terms: Seq[RealNumber]): Seq[RealNumber] = {
        var simplified = false
        var current = term
        val product = terms.foldLeft(new mutable.ListBuffer[RealNumber]())((seq, next) => next ?*? current match {
            case Some(m) => {
                simplified = true
                current = m
                seq += current
            }
            case _ => seq :+ next
        })
        if (!simplified) current +=: product
        product.toSeq
    }

    def isEmpty = product.isEmpty

    override def toString = product.toString

}