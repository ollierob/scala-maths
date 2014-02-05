package net.ollie.maths.functions.numeric

import net.ollie.maths.{Number, Variable}
import net.ollie.maths.numbers._
import net.ollie.maths.numbers.real.EulersNumber
import org.junit.runner.RunWith
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.junit.JUnitRunner

/**
 * Created by Ollie on 16/01/14.
 */
@RunWith(classOf[JUnitRunner])
class LnTest extends FlatSpec with Matchers {

    import net.ollie.maths.numbers.Precision._

    "Ln(1)" should "be 0" in {
        Ln(One) shouldBe Zero
    }

    "Ln(2)" should "be 0" in {
        Ln(2).evaluate(4 dp) shouldBe BigDecimal("0.6931")
    }

    "Ln(4)" should "evaluate" in {
        val r = Ln(4)
        r.evaluate(4 dp) shouldBe BigDecimal("1.3863")
        r == One shouldBe false
    }

    "1 / Ln(4)" should "evaluate" in {
        val r = 1 / Ln(4)
        r.evaluate(4 dp) shouldBe BigDecimal("0.7213")
    }

    "4 * Ln(4)" should "evaluate" in {
        val r = 4 * Ln(4)
        r.evaluate(4 dp) shouldBe BigDecimal("5.5452")
    }

    "1 / (4 * Ln(4))" should "evaluate" in {
        val r = One / (4 * Ln(4))
        println(r)
        println(r.getClass)
        r.evaluate(4 dp) shouldBe BigDecimal("0.1803")
    }

    "Ln(Ln(4))" should "evaluate" in {
        val r: Number = Ln(Ln(4))
        val re: Real = Real(r).get
        re.evaluate(4 dp) shouldBe BigDecimal("0.3266")
    }

    val x = Variable("x")

    behavior of "Ln(Ln(x))"

    {

        val f = Ln(Ln(x))

        it should "evaluate e" in {
            f.replace(x, EulersNumber).toConstant shouldBe Some(Zero)
        }

        it should "differentiate" in {
            val df = f.df(x)
            df.replace(x, 4).toConstant shouldBe Some(1 / (4 * Ln(4)))
            val n = 1 / (4 * Ln(4))
        }

    }

    "Ln(2x)" should "differentiate to 1/x" in {
        val two = Integer(2)
        val ln = Ln(two * x)
        ln.df(x).replace(x, One).toConstant shouldBe (Some(One))
    }

    "Ln(Exp(x))" should "be x" in {
        Ln(Exp(x)) shouldBe x
    }

}
