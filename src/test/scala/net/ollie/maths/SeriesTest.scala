package net.ollie.maths

import org.scalatest.{Matchers, FlatSpec}
import net.ollie.maths.numbers.RealNumber
import net.ollie.maths.numbers.complex.ComplexNumber
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 12/01/14.
 */
@RunWith(classOf[JUnitRunner])
class SeriesTest extends FlatSpec with Matchers {

    behavior of "Expression series"

    it should "convert to constant" in {
        val x = Variable("x")
        val y = Variable("y")
        val product = Series(Seq(x, y))
        product.toConstant shouldBe (None)
        val r = RealNumber(3)
        val z = ComplexNumber(5, 7)
        product.replace(Map(x -> r, y -> z)).toConstant shouldBe (Some(ComplexNumber(8, 7)))
        product.replace(Map(x -> z, y -> r)).toConstant shouldBe (Some(ComplexNumber(8, 7)))
    }

    it should "not convert to constant" in {
        val x = Variable("x")
        val y = Variable("y")
        val series = Series(Seq(x, y))
        val r = RealNumber(3)
        series.replace(x, r).toConstant shouldBe (None)
        series.replace(y, r).toConstant shouldBe (None)
        series.replace(Map(x -> r, y -> r)).toConstant shouldBe (Some(r + r))
    }

}
