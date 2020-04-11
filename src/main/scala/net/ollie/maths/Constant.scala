package net.ollie.maths

import net.ollie.maths.expressions.{Empty, Expression, Integrable, Invertible, Multiplied, Nonvariate}
import net.ollie.maths.numbers.constants.{One, Unity, Zero}

import scala.collection.mutable

/**
 * Created by Ollie on 02/01/14.
 */
trait Constant
    extends Nonvariate
        with Invertible
        with Integrable {

    type System >: this.type <: Constant

    def narrow: System = this

    def unary_-(): System

    def inverse: Constant

    def isZero = isEmpty

    override def ?+(that: Expression)(leftToRight: Boolean): Option[Expression] = that.toConstant match {
        case Some(n) => this ?+ n
        case _ => super.?+(that)(leftToRight)
    }

    def ?+(that: Constant): Option[Constant]

    def +[R <: Constant, Combined <: Constant](that: R)
                                              (implicit addition: AdditionArithmetic[System, R#System, Combined]): Combined = {
        addition.add(this, that.narrow)
    }

    def -[R <: Constant, Combined <: Constant](that: R)
                                              (implicit addition: AdditionArithmetic[System, R#System, Combined]): Combined = {
        addition.add(this, -that)
    }

    override def ?*(that: Expression)(leftToRight: Boolean) = that match {
        case n: Constant => this.?*(n)(leftToRight)
        case _ => super.?*(that)(leftToRight)
    }

    final def ?*?(that: Constant): Option[Constant] = this.?*(that)(true) match {
        case Some(n) => Some(n)
        case _ => that.?*(this)(false) match {
            case Some(n) => Some(n)
            case _ => None
        }
    }

    def ?*(that: Constant)(leftToRight: Boolean): Option[Constant]

    def *[R <: Constant, Combined <: Constant](that: R)
                                              (implicit multiplication: MultiplicationArithmetic[System, R#System, Combined]): Combined = {
        multiplication.multiply(this, that.narrow)
    }

    /**
     * Exponentiation.
     *
     * @param power
     * @param exponentiation
     * @tparam R        power type
     * @tparam Combined result type
     * @return a number
     */
    def ^[R <: Constant, Combined <: Constant](power: R)
                                              (implicit exponentiation: ExponentiationArithmetic[System, R#System, Combined]): Combined = {
        exponentiation.exponentiate(this, power.narrow)
    }

    def ?^(that: Constant): Option[Constant]

    /**
     * Tetration.
     *
     * @param tower
     * @param tetration
     * @tparam R
     * @tparam Combined
     * @return
     * @see http://mathworld.wolfram.com/Tetration.html
     */
    def ^^[R <: Constant, Combined <: Constant](tower: R)
                                               (implicit tetration: TetrationArithmetic[System, R#System, Combined]): Combined = {
        tetration.tetrate(this, tower.narrow)
    }

    override def df(x: Variable): Constant with Empty = Zero

    def toConstant: Option[System] = Some(narrow)

    override def replace(variables: Map[Variable, Expression]): System = narrow

    final override def equals(expr: Expression) = expr.toConstant match {
        case Some(n) => this.equals(n)
        case _ => super.equals(expr)
    }

    def equals(n: Constant) = super.equals(n)

}

object Constant {

    implicit object ConstantArithmetic
        extends MultiplicationArithmetic[Constant#System, Constant#System, Constant] {

        def one: Constant with Unity = One

        def multiply(left: Constant#System, right: Constant#System): Constant = {
            left.?*?(right) match {
                case Some(n) => n
                case _ => new MultipliedConstant(left, right)
            }
        }

    }

    private class NegatedConstant(val of: Constant)
        extends Constant {

        type System = Constant

        def isEmpty = of.isEmpty

        def ?^(that: Constant) = None

        def ?*(that: Constant)(leftToRight: Boolean) = None

        def ?+(that: Constant) = None

        def inverse = -(of.inverse)

        def unary_-() = of

        override def toString = s"-$of"

    }

    private class MultipliedConstant(val left: Constant, val right: Constant)
        extends Constant
            with Multiplied {

        type System = Constant

        def ?^(that: Constant) = None

        def ?*(that: Constant)(leftToRight: Boolean) = {
            if (leftToRight) that.?*(left)(true)
            else that.?*(right)(true)
        }

        def ?+(that: Constant) = None

        def inverse = new InvertedConstant(this)

        def unary_-() = new NegatedConstant(this)

        override def df(x: Variable) = super[Constant].df(x)

    }

    private class InvertedConstant(val inverse: Constant)
        extends Constant {

        type System = Constant

        def isEmpty = false

        def ?^(that: Constant) = inverse ?^ (-that)

        def ?*(that: Constant)(leftToRight: Boolean) = None

        def ?+(that: Constant) = None

        def unary_-() = (-inverse).inverse

    }

}

abstract class ConstantSeries[N <: Constant](val terms: Seq[N])
    extends Constant {

    require(terms.nonEmpty)

    protected[this] def simplify(terms: Seq[N]): Seq[N] = {
        var simplified = false
        var current = terms.head
        val series = terms.tail.foldLeft(new mutable.ListBuffer[N]())((seq, next) => tryAdd(next, current) match {
            case Some(m) => {
                simplified = true
                current = m
                seq += current
            }
            case _ => seq :+ next
        })
        if (!simplified) series += current;
        series.toSeq
    }

    protected[this] def tryAdd(left: N, right: N): Option[N]

    def isEmpty = terms.forall(_.isEmpty)

    override def toString = terms.mkString("(", " + ", ")")

}

abstract class ConstantProduct[N <: Constant](val terms: Seq[N])
                                             (implicit identity: NumberIdentityArithmetic[N])
    extends Constant
        with Multiplied {

    require(terms.nonEmpty)

    def left: N = terms.head

    def right: N = terms match {
        case head :: Nil => identity.one
        case head :: tail => apply(tail)
    }

    protected[this] def apply(terms: Seq[N]): N

    protected[this] def simplify(terms: Seq[N]): Seq[N] = {
        var simplified = false
        var current = terms.head
        val series = terms.tail.foldLeft(new mutable.ListBuffer[N]())((seq, next) => tryMultiply(next, current) match {
            case Some(m) =>
                simplified = true
                current = m
                seq += current
            case _ => seq :+ next
        })
        if (!simplified) series += current;
        series.toSeq
    }

    protected[this] def tryMultiply(left: N, right: N): Option[N]

    override def isEmpty = terms.exists(_.isEmpty)

    override def df(x: Variable) = super[Constant].df(x)

    override def hashCode = terms.hashCode

    override def toString = terms.mkString("(", " * ", ")")

}