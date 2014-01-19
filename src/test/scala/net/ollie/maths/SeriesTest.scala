package net.ollie.maths

import net.ollie.maths.numbers.RealNumber
import net.ollie.maths.numbers.complex.ComplexNumber
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 12/01/14.
 */
@RunWith(classOf[JUnitRunner])
class SeriesTest extends FlatSpec with Matchers {

    val x = Variable("x")
    val y = Variable("y")

    behavior of "x + y"

    it should "convert to constant" in {
        val series = Series(Seq(x, y))
        series.toConstant shouldBe (None)
        val r = RealNumber(3)
        val z = ComplexNumber(5, 7)
        series.replace(Map(x -> r, y -> z)).toConstant shouldBe (Some(ComplexNumber(8, 7)))
        series.replace(Map(x -> z, y -> r)).toConstant shouldBe (Some(ComplexNumber(8, 7)))
    }

    it should "not convert to constant" in {
        val series = Series(Seq(x, y))
        val r = RealNumber(3)
        series.replace(x, r).toConstant shouldBe (None)
        series.replace(y, r).toConstant shouldBe (None)
        series.replace(Map(x -> r, y -> r)).toConstant shouldBe (Some(r + r))
    }

}
