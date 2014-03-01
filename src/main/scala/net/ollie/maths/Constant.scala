package net.ollie.maths

import scala.collection.mutable
import net.ollie.maths.numbers.constants.Zero

/**
 * Created by Ollie on 02/01/14.
 */
trait Constant
        extends Nonvariate
        with Invertible
        with Integrable {

    self =>

    type System >: this.type <: Constant

    final def narrow: System = this

    def unary_-(): System

    def inverse: Constant

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
     * @param power
     * @param exponentiation
     * @tparam R power type
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
     * @param tower
     * @param tetration
     * @tparam R
     * @tparam Combined
     * @return
     * @see http://mathworld.wolfram.com/PowerTower.html
     */
    def ^^[R <: Constant, Combined <: Constant](tower: R)
            (implicit tetration: TetrationArithmetic[System, R#System, Combined]): Combined = {
        tetration.tetrate(this, tower.narrow)
    }

    override def df(x: Variable): EmptyConstant = Zero

    override protected[this] def integral(x: Variable) = this * x

    def toConstant: Option[System] = Some(narrow)

    override def replace(variables: Map[Variable, Expression]): System = narrow

    final override def equals(expr: Expression) = expr match {
        case n: Constant => this.equals(n)
        case _ => expr.toConstant match {
            case Some(n: Constant) => this.equals(n)
            case _ => super.equals(expr)
        }
    }

    def equals(n: Constant) = super.equals(n)

}

abstract class ConstantSeries[N <: Constant](val terms: Seq[N])
        extends Constant {

    require(!terms.isEmpty)

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
        extends Constant {

    require(!terms.isEmpty)

    protected[this] def simplify(terms: Seq[N]): Seq[N] = {
        var simplified = false
        var current = terms.head
        val series = terms.tail.foldLeft(new mutable.ListBuffer[N]())((seq, next) => tryMultiply(next, current) match {
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

    protected[this] def tryMultiply(left: N, right: N): Option[N]

    def isEmpty = terms.find(_.isEmpty).isDefined

    override def toString = terms.mkString("(", " * ", ")")

}