package net.ollie.maths.functions.numeric

import scala.collection.mutable

import net.ollie.maths._
import net.ollie.maths.numbers.{IntegerNumber, NaturalNumber, Zero}

/**
 * Created by Ollie on 19/01/14.
 */
object Sum {

    def apply(f: (IntegerNumber) => Differentiable, start: NaturalNumber, end: NaturalNumber): Differentiable = {
        if (end < start) Zero
        else new Sum(f, start, end)
    }

}

class Sum(f: (IntegerNumber) => Differentiable, start: IntegerNumber, end: IntegerNumber)
        extends Differentiable {

    private val size = (end - start).toInt.get

    private lazy val series: Differentiable = {
        val terms: mutable.ArrayBuffer[Differentiable] = new mutable.ArrayBuffer[Differentiable](size)
        for (i <- 0 to size) terms += f(start + i)
        Differentiable.series(terms)
    }

    def df(x: Variable) = series.df(x)

    def replace(variables: Map[Variable, Expression]) = series.replace(variables)

    def toConstant = series.toConstant

    def variables = series.variables

    def isEmpty = series.isEmpty

    override def toString = s"Σ($start:$end)($f)"

}