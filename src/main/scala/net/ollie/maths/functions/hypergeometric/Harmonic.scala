package net.ollie.maths.functions.hypergeometric

import net.ollie.maths.numbers.{Integer, Precision, Natural, Real}
import net.ollie.maths.numbers.constants.{One, EulerMascheroniConstant}
import net.ollie.maths.functions.UnivariateFunction
import net.ollie.maths.methods.Series
import net.ollie.maths.CachedEvaluated

/**
 * Created by Ollie on 17/02/14.
 * @see http://mathworld.wolfram.com/HarmonicNumber.html
 */
object Harmonic
        extends UnivariateFunction[Natural, Real] {

    def apply(degree: Natural): Real with Harmonic = {
        if (degree < 10) new SmallRealHarmonic(degree)
        else new LargeRealHarmonic(degree)
    }

}

trait Harmonic
        extends GeneralizedHarmonic {

    def power = One

    override def toString = s"Harmonic($degree)"

}

class SmallRealHarmonic(val degree: Natural)
        extends Real
        with Harmonic
        with CachedEvaluated {

    def isEmpty = false

    private lazy val series: Real = Series(nth _, One, degree)

    private def nth(i: Integer): Real = 1 / i

    protected[this] override def doEvaluate(precision: Precision) = series.evaluate(precision)

}

class LargeRealHarmonic(val degree: Natural)
        extends Real
        with Harmonic
        with CachedEvaluated {

    require(!degree.isEmpty)

    def isEmpty = false

    private lazy val repr: Real = EulerMascheroniConstant + Digamma(degree + 1)

    protected[this] override def doEvaluate(precision: Precision) = repr.evaluate(precision)

}
