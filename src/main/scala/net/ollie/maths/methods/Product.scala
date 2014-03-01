package net.ollie.maths.methods

import scala.Some
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import net.ollie.maths._
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.constants.{Zero, One}

/**
 * Created by Ollie on 19/01/14.
 */
object Product {

    def apply(left: Expression, right: Expression): Expression = if (left.isEmpty || right.isEmpty) Zero else new Product(Seq(left, right))

    def apply(expressions: Seq[Expression]): Expression = expressions match {
        case Nil => Zero
        case expression :: Nil => expression
        case _ => new Product(expressions)
    }

    def apply[N <: Number](f: (Integer) => N, range: Range)(implicit multiplication: MultiplicationArithmetic[N#System, N#System, N#System]): N#System = {
        var product: N#System = multiplication.one
        for (j <- range) {
            product = multiplication.multiply(product, f(j).narrow)
        }
        product
    }

    def apply[N <: Number](f: (Integer) => N, min: Integer, max: Integer)(implicit multiplication: MultiplicationArithmetic[N#System, N#System, N#System]): N#System = {
        apply(f, min.toInt.get to max.toInt.get)(multiplication)
    }

    def apply(fn: (Natural) => Real, start: Natural): Real = new InfiniteRealProduct(fn, start)

}

class Product[+T <: Expression](val terms: Seq[T])
        extends Aggregate {

    require(!terms.isEmpty)

    override def ?*(that: Expression)(leftToRight: Boolean) = Some(if (leftToRight) tailTimes(that) else headTimes(that))

    /**
     * Tail * that
     * @param that
     * @return
     */
    protected[this] def tailTimes(that: Expression): Expression = terms.last.?*(that)(true) match {
        case Some(x) => apply(terms.dropRight(1) :+ x)
        case _ => that match {
            case p: Product[_] => apply(terms ++: p.terms)
            case _ => apply(terms :+ that)
        }
    }

    /**
     * That * head
     * @param that
     * @return
     */
    protected[this] def headTimes(that: Expression): Expression = that.?*(terms.head)(true) match {
        case Some(x) => apply(x +: terms.tail)
        case _ => that match {
            case p: Product[_] => apply(p.terms ++: terms)
            case _ => apply(that +: terms)
        }
    }

    protected[this] def simplify(terms: Seq[Expression]): Seq[Expression] = {
        val simplified = new ListBuffer[Expression]()
        var head: Expression = null
        terms.tail.foldLeft(terms.head)((result, current) => {
            result ?*? current match {
                case Some(m) => {
                    head = m
                    m
                }
                case _ => {
                    simplified += result
                    head = current
                    current
                }
            }
        })
        simplified += head
        simplified.toSeq
    }

    protected[this] def apply(expressions: Seq[Expression]) = {
        Product(simplify(expressions))
    }

    override def ?/(that: Expression) = {
        terms.last ?/ that match {
            case Some(x) => Some(apply(terms.init :+ x))
            case _ => super.?/(that)
        }
    }

    def isEmpty = terms.exists(_.isEmpty)

    def toConstant: Option[Number] = {
        val constants = terms.map(_.toConstant)
        val one: Option[Number] = Some(One)
        constants.foldLeft(one)(multiply)
    }

    private def multiply(result: Option[Number], current: Option[Number]): Option[Number] = {
        if (!result.isDefined || !current.isDefined) return None
        result.get ?*? current.get
    }

    def df(x: Variable) = {
        var sum: Expression = Zero
        for (i <- 0 to terms.length - 1) {
            val d: Expression = terms(i).df(x)
            if (!d.isEmpty) sum = sum + apply(terms.updated(i, d))
        }
        sum
    }

    override def toString = terms.mkString("(", " * ", ")")

    override def hashCode = terms.hashCode

}

class InfiniteRealProduct(f: (Natural) => Real, start: Natural)
        extends Real
        with IterativelyEvaluated {

    def evaluationIterator(startPrecision: Precision) = new EvaluationIterator() {

        private val terms: ArrayBuffer[Real] = new ArrayBuffer[Real]()

        def next(nth: Natural, precision: Precision) = {
            val n: Natural = nth + start
            terms += f(n)
            terms.map(_.approximatelyEvaluate(precision)).product
        }

    }

    def isEmpty = f(start).isEmpty || f(start.succ).isEmpty //TODO more terms?

}