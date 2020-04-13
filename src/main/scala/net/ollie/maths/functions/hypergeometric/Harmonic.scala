package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.CachedEvaluated
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.methods.Series
import net.ollie.maths.numbers.constants.{EulerMascheroniConstant, One}
import net.ollie.maths.numbers.{Integer, Natural, Precision, Real}

/**
 * Created by Ollie on 17/02/14.
 *
 * @see http://mathworld.wolfram.com/HarmonicNumber.html
 */
trait Harmonic
    extends GeneralizedHarmonic {

    override def power = One

    override def isEmpty = false

    override def toString = s"Harmonic($degree)"

}

object Harmonic
    extends UnivariateFunction[Natural, Real] {

    def apply(degree: Natural): Real with Harmonic = degree match {
        case One => HarmonicOne
        case _ if degree < 10 => new SmallRealHarmonic(degree)
        case _ => new LargeRealHarmonic(degree)
    }

}

private object HarmonicOne
    extends Real
        with Harmonic {

    override def evaluate(precision: Precision) = One.BIG_DECIMAL

    override def degree = One

}

class SmallRealHarmonic(val degree: Natural)
    extends Real
        with Harmonic
        with CachedEvaluated {

    private lazy val series: Real = Series(nth _, One, degree)

    private def nth(i: Integer): Real = 1 / i

    protected[this] override def doEvaluate(precision: Precision) = series.evaluate(precision)

}

class LargeRealHarmonic(val degree: Natural)
    extends Real
        with Harmonic
        with CachedEvaluated {

    require(!degree.isEmpty)

    private lazy val repr: Real = EulerMascheroniConstant + Digamma(degree + 1)

    protected[this] override def doEvaluate(precision: Precision) = repr.evaluate(precision)

}
