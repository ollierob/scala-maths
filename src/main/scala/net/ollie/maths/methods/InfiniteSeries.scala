package net.ollie.maths.methods

import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.RoundingMode

import net.ollie.maths.numbers.{NaturalNumber, Precision, RealNumber}

/**
 * Created by Ollie on 16/01/14.
 */
object InfiniteSeries {

    def apply(f: (NaturalNumber) => RealNumber, start: NaturalNumber): RealNumber = new InfiniteSeries(f, start)

}

class InfiniteSeries(f: (NaturalNumber) => RealNumber, start: NaturalNumber)
        extends RealNumber
        with IterativelyEvaluated {

    def isEmpty = false;

    def evaluationIterator = new EvaluationIterator() {

        val terms: ListBuffer[RealNumber] = new ListBuffer[RealNumber]()

        def next(nth: NaturalNumber, precision: Precision)(implicit mode: RoundingMode.RoundingMode) = {
            val n: NaturalNumber = nth + start
            terms += f(n)
            terms.map(_.approximatelyEvaluate(precision)).sum
        }

    }

}