package net.ollie.maths.numbers.real.surreal

import net.ollie.maths.{EmptyNumber, Variable}
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 06/01/14.
 * @see http://mathworld.wolfram.com/SurrealNumber.html
 */
trait Surreal
        extends Real {

    def nearest: Real

    override def abs = nearest.abs

    protected[this] def eval(precision: Precision) = nearest.evaluate(precision)

    def isEmpty = left.isEmpty && right.isEmpty

    def left: SurrealSet

    def right: SurrealSet

    def isNumeric = !left.intersects(right) && ???

    override def ?+(that: Real) = that match {
        case s: Surreal => Some(this + s)
        case _ => super.?+(that)
    }

    def +(that: Surreal): Surreal = Surreal((this.left + that) :: (this + that.right), (this.right + that) :: (this + that.right))

    def +(that: SurrealSet): SurrealSet = that + this

    def -(that: Surreal): Surreal = this + (-that)

    override def tryCompareTo(that: Real) = that match {
        case s: Surreal => this.tryCompareTo(s)
        case _ => super.tryCompareTo(that)
    }

    def tryCompareTo(s: Surreal): Option[Int] = ???

    override def unary_-(): Surreal = Surreal(-right, -left)

    override def df(x: Variable) = EmptyForm

    override def toString = s"{$left | $right}"

}

object Surreal {

    def apply(): Surreal = EmptyForm

    implicit def apply(re: Real): Surreal = re match {
        case s: Surreal => s
        case _ => new WrapperForm(re)
    }

    def apply(left: SurrealSet, right: SurrealSet): Surreal = {
        if (left.isEmpty && right.isEmpty) Surreal()
        else new RegularForm(left, right)
    }

    implicit object SurrealArithmetic
            extends Ordering[Surreal] {

        def compare(x: Surreal, y: Surreal) = ???

    }

    implicit class SurrealBuilder(left: Real) {

        def |(right: SurrealSet): Surreal = Surreal(left, right)

    }

}

/**
 * Surreal form of zero.
 */
object EmptyForm
        extends Surreal
        with EmptyNumber {

    def left = SurrealSet()

    def right = SurrealSet()

    def nearest = Zero

    override def abs = Zero

    override def +(that: Surreal) = that

    override def unary_-() = this

    override def toConstant = Some(this)

    override def variables = Set()

    override def isEmpty = true

    override def eval(precision: Precision) = super[EmptyNumber].eval(precision)

    override def toString = super[EmptyNumber].toString

}

object InfiniteForm
        extends Surreal
        with Infinite {

    override def abs = super[Infinite].abs

    override def isEmpty = super[Infinite].isEmpty

    def nearest = Infinity

    protected[this] override def eval(precision: Precision) = Infinity.evaluate(precision)

    def left = ???

    def right = SurrealSet()

    override def +(that: Surreal) = this

    override def toString = "ω"

}

object InfinitesimalForm
        extends Surreal {

    def nearest = Zero

    def left = Zero

    def right = ???

    override def toString = "ε"

}

private class WrapperForm(val re: Real)
        extends Surreal {

    require(!re.isInstanceOf[Surreal])

    def nearest = re

    def left: SurrealSet = ???

    def right: SurrealSet = ???

}

private class RegularForm(val left: SurrealSet, val right: SurrealSet)
        extends Surreal {

    require(!(left.isEmpty && right.isEmpty))

    def nearest = (left, right) match {
        case _ if left.isEmpty => right.min - 1
        case _ if right.isEmpty => left.max + 1
        case _ => (left.max + right.min) / 2
    }

}
