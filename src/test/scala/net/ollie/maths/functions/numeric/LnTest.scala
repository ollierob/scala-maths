package net.ollie.maths.functions.numeric

import org.scalatest.{Matchers, FlatSpec}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import net.ollie.maths.Variable
import net.ollie.maths.numbers.RealNumber

/**
 * Created by Ollie on 16/01/14.
 */
@RunWith(classOf[JUnitRunner])
class LnTest extends FlatSpec with Matchers {

    import net.ollie.maths.numbers.Precision._

    "Ln(5)" should "evaluate" in {
        val r = Ln(5)
        r.evaluate(4 dp) shouldBe (BigDecimal("1.6094"))
    }

    "Ln(Ln(x))" should "differentiate" in {
        val x = Variable("x")
        val r = Ln(Ln(x))
        val df = r.df(x) //Should be 1/(x ln(x))
        val re: RealNumber = df.replace(x, 4).toConstant.asInstanceOf
        re.evaluate(4 dp) shouldBe (BigDecimal("0.1803"))
    }

}
