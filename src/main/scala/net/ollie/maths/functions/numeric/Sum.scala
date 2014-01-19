package net.ollie.maths.functions.numeric

import scala.collection.mutable

import net.ollie.maths._
import net.ollie.maths.numbers.{NaturalNumber, Zero}

/**
 * Created by Ollie on 19/01/14.
 */
object Sum {

    def apply(f: (NaturalNumber) => Differentiable, start: NaturalNumber, end: NaturalNumber): Differentiable = {
        val from = start.toInt.get //TODO operate on NaturalNumber instead?
        val to = end.toInt.get
        if (from > to) Zero else new Sum(f, from, to)
    }

}

class Sum(f: (NaturalNumber) => Differentiable, from: Int, to: Int)
        extends Differentiable {

    private lazy val series: Differentiable = {
        val terms: mutable.ArrayBuffer[Differentiable] = new mutable.ArrayBuffer[Differentiable](to - from + 1)
        for (i <- from to to) terms += f(i)
        Differentiable.series(terms)
    }

    def df(x: Variable) = series.df(x)

    def replace(variables: Map[Variable, Expression]) = series.replace(variables)

    def toConstant = series.toConstant

    def variables = series.variables

    def isEmpty = series.isEmpty

    override def toString = s"Σ($from:$to)($f)"

}