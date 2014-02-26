package net.ollie.maths.numbers

import scala.Some

import net.ollie.maths._
import net.ollie.maths.methods.ApproximatelyEvaluated
import net.ollie.maths.numbers.massive.{PowerTower, Massive}
import net.ollie.maths.numbers.constants.{Unity, Zero, One}

/**
 * Created by Ollie on 01/01/14.
 */
trait Real
        extends Number
        with Ordered[Real]
        with MaybeReal
        with Evaluable {

    type System = Real

    override def replace(variables: Map[Variable, Expression]) = this

    override def unary_-(): Real = Real.negate(this)

    override def df(x: Variable): Real with EmptyNumber = Zero

    def inverse: Real = Real.inverse(this)

    def abs: PositiveReal = Real.abs(this)

    def +(that: Real): Real = this ?+ that match {
        case Some(n) => n
        case _ => that ?+ this match {
            case Some(m) => m
            case _ => RealSeries(this, that)
        }
    }

    def ?+(that: Real): Option[Real] = that match {
        case Zero => Some(this)
        case _ => None
    }

    def ?+?(that: Real): Option[Real] = this ?+ that match {
        case Some(m) => Some(m)
        case _ => that ?+ this match {
            case Some(m) => Some(m)
            case _ => None
        }
    }

    def ?+(that: Number) = that match {
        case re: Real => Some(this + re)
        case _ => None
    }

    def -(that: Real): Real = this + (-that)

    def *(that: Real): Real = {
        this ?* that match {
            case Some(n) => n
            case _ => that ?* this match {
                case Some(m) => m
                case _ => RealProduct(this, that)
            }
        }
    }

    def ?*(that: Real): Option[Real] = that match {
        case Zero => Some(Zero)
        case One => Some(this)
        case _ => None
    }

    /**
     * Shortcut for this ?* that and that ?* this. Note that real multiplication is commutative.
     * @param that
     * @return
     */
    def ?*?(that: Real): Option[Real] = this ?* that match {
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
    def ?*(that: Number)(leftToRight: Boolean) = that match {
        case re: Real => Some(this * re)
        case _ => None
    }

    def /(that: Real): Real = this * that.inverse

    def ^(that: Integer): Real = Real.pow(this, that)

    def ?^(that: Number): Option[Number] = that match {
        case int: Integer => Some(this ^ int)
        case re: Real if this.isStrictlyPositive => ??? // Some(this.abs ^ re) //TODO multivalued
        case _ => None
    }

    def squared: PositiveReal = (this ^ 2).abs

    def compare(that: Real): Int = {
        if (this eq that) 0
        else this.tryCompareTo(that) match {
            case Some(i) => i
            case _ => that.tryCompareTo(this) match {
                case Some(j) => -j
                case _ => this.evaluate(SinglePrecision).compare(that.evaluate(SinglePrecision)) //TODO this should return 0
            }
        }
    }

    def isStrictlyPositive: Boolean = Zero < this

    def toReal = Some(this)

    protected def tryCompareTo(that: Real): Option[Int] = None

    override def equals(number: Number) = number match {
        case re: Real => this.equals(re)
        case _ => super.equals(number)
    }

    def equals(that: Real): Boolean = {
        super.equals(that) || (this ?== that match {
            case Some(b) => b
            case _ => that ?== this match {
                case Some(b) => b
                case _ => this ~= that
            }
        })
    }

    /**
     * Try-equals.
     */
    def ?==(that: Real): Option[Boolean] = None

    /**
     * Approximately-equals.
     */
    def ~=(that: Real)(implicit precision: Precision = DoublePrecision): Boolean = {
        this.tryEvaluate(precision) == that.tryEvaluate(precision)
    }

}

object Real
        extends NumberIdentityArithmetic[Real] {

    private val BD_ZERO = BigDecimal(0)

    def zero = Zero

    def one = One

    def apply(from: Number): Option[Real] = from match {
        case re: Real => Some(re)
        case m: MaybeReal => m.toReal
        case _ => None
    }

    implicit def apply(int: Int): Real = Integer(int)

    def apply(d: Double): Real = if (d == 0d) Zero else new ExactDouble(d)

    def apply(bd: BigDecimal): Real = if (bd == BD_ZERO) Zero else new ExactBigDecimal(bd)

    def negate(re: Real) = if (re.isEmpty) Zero else new NegatedReal(re)

    def inverse(re: Real) = new InverseReal(re)

    def abs(re: Real): PositiveReal = re match {
        case Zero => Zero
        case p: PositiveReal => p
        case _ => new AbsReal(re)
    }

    def pow(base: Real, power: Integer): Real = RealPower(base, power)

    implicit object RealArithmetic
            extends AdditionArithmetic[Real, Real, Real]
            with MultiplicationArithmetic[Real, Real, Real]
            with ExponentiationArithmetic[Real, Real, RealPower]
            with TetrationArithmetic[Real, Real, Massive]
            with IdentityArithmetic[Real, Real]
            with scala.math.Numeric[Real] {

        override def zero: Real with Empty = Zero

        override def one: Real with Unity = One

        def add(x: Real, y: Real) = plus(x, y)

        def plus(x: Real, y: Real) = x + y

        def minus(x: Real, y: Real) = x - y

        def times(x: Real, y: Real) = ???

        def negate(x: Real) = -x

        def fromInt(x: Int) = Real(x)

        def toInt(x: Real) = x.tryEvaluate(IntegerPrecision).get.toInt

        def toLong(x: Real) = x.tryEvaluate(IntegerPrecision).get.toLong

        def toFloat(x: Real) = x.tryEvaluate(SinglePrecision).get.toFloat

        def toDouble(x: Real) = x.tryEvaluate(DoublePrecision).get.toDouble

        def compare(x: Real, y: Real) = x.compareTo(y)

        def multiply(left: Real, right: Real) = left * right

        def exponentiate(base: Real, power: Real) = RealPower(base, power)

        def tetrate(base: Real, tower: Real) = PowerTower(base, tower)

        def promote(from: Real) = from

    }

}

trait MaybeReal {

    def toReal: Option[Real]

    //    def tryEvaluate(precision: Precision): OptionalBigDecimal = toReal match {
    //        case Some(re) => re.tryEvaluate(precision)
    //        case _ => None
    //    }

}

class ExactDouble(val of: Double)
        extends AnyRef
        with Real {

    def isEmpty = of == 0d

    def evaluate(precision: Precision) = precision(of)

    override def toString = of.toString

}

class ExactBigDecimal(val of: BigDecimal)
        extends AnyRef
        with Real {

    override def ?+(that: Real) = that match {
        case re: ExactBigDecimal => Some(Real(this.of + re.of))
        case _ => super.?+(that)
    }

    override def ?*(that: Real) = that match {
        case re: ExactBigDecimal => Some(Real(this.of * re.of))
        case _ => super.?*(that)
    }

    def isEmpty = of == 0

    def evaluate(precision: Precision) = precision(of)

    override def toString = of.toString

}

class NegatedReal(val of: Real)
        extends Real {

    def evaluate(precision: Precision) = -(of.evaluate(precision))

    override def variables = super[Real].variables

    override def unary_-() = of

    override def toConstant = Some(this)

    def isEmpty = of.isEmpty

    override def toString = s"-($of)"

    override def hashCode = -of.hashCode

}

class InverseReal(val of: Real)
        extends Real
        with ApproximatelyEvaluated {

    require(!of.isEmpty)

    override def inverse = of

    def isEmpty = false

    override def ?*(that: Real) = that match {
        case real: Real if of == real => Some(One)
        case _ => super.?*(that)
    }

    override def doApproximatelyEvaluate(precision: Precision) = 1 / of.evaluate(precision)

    override def toString = s"1/$of"

}

class AbsReal(val of: Real)
        extends PositiveReal
        with CachedEvaluated {

    def isEmpty = of.isEmpty

    override def toString = s"|$of|"

    protected[this] def doEvaluate(precision: Precision) = of.evaluate(precision).abs

}
