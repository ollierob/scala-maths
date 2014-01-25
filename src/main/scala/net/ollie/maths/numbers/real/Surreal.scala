package net.ollie.maths.numbers.real

import net.ollie.maths.{Empty, Variable}
import net.ollie.maths.numbers._

/**
 * Created by Ollie on 06/01/14.
 */
trait Surreal
        extends Real {

    def nearest: Real

    override def abs = nearest.abs

    override def unary_-(): Surreal = ???

    protected[this] def eval(precision: Precision) = nearest.evaluate(precision)

    override def df(x: Variable) = EmptyForm

}

trait SurrealForm extends Surreal {

    def left: NumberSet

    def right: NumberSet

    override def unary_-(): SurrealForm = ???

    override def toString = "{ " + left.toString + " | " + right.toString + " }"

}

object EmptyForm
        extends SurrealForm
        with Empty {

    def left = NumberSet()

    def right = NumberSet()

    def nearest = Zero

    override def unary_-() = this

    override def variables = Set()

    override def isEmpty = true

    override def df(x: Variable) = super[SurrealForm].df(x)

    override def toString = "{ | }"

}

object TransfiniteForm
        extends Surreal
        with Infinite {

    override def abs = super[Infinite].abs

    override def unary_-() = ???

    def nearest = Infinity

    protected[this] override def eval(precision: Precision) = Infinity.evaluate(precision)

    override def toString = "ω"

}

object InfinitesimalForm
        extends Surreal {

    def isEmpty = false

    override def toString = "ε"

    def nearest = Zero

}

class RegularForm(val left: NumberSet, val right: NumberSet) extends SurrealForm {

    def isEmpty = left.isEmpty && right.isEmpty

    def nearest = ???

    override def unary_-(): SurrealForm = new RegularForm(-right, -left)

}
