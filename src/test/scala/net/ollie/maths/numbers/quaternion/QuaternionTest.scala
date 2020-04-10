package net.ollie.maths.numbers.quaternion

import net.ollie.maths.numbers.complex.Complex
import net.ollie.maths.numbers.constants.One
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by Ollie on 11/01/14.
 */
class QuaternionTest extends FlatSpec with Matchers {

    val i = Quaternion.i
    val j = Quaternion.j
    val k = Quaternion.k

    behavior of "i"

    {

        it should "equal itself" in {
            i == i shouldBe true
        }

        it should "multiply by i" in {
            i * i shouldBe -One
        }

        it should "multiply by j" in {
            i * j shouldBe k
        }

        it should "multiply by k" in {
            i * k shouldBe -j
        }

        it should "norm" in {
            i.norm shouldBe One
            i.abs shouldBe One
        }

        it should "invert" in {
            i.inverse shouldBe -i
        }

        it should "add to real" in {
            i + 5 shouldBe Quaternion(5, 1, 0, 0)
            5 + i shouldBe Quaternion(5, 1, 0, 0)
        }

    }

    behavior of "j"

    {

        it should "multiply by i" in {
            j * i shouldBe (-k)
        }

        it should "multiply by j" in {
            j * j shouldBe -One
        }

    }

    "i * j * k" should "be -1" in {
        i * j * k shouldBe -One
    }

    "i * j / i" should "be -j" in {
        i * j / i shouldBe -j
    }

    behavior of "2 + 3i + 5j + 7k"

    {

        val q = Quaternion(2, 3, 5, 7)

        it should "equal itself" in {
            (q == q) shouldBe (true)
        }

        it should "add 11" in {
            q + 11 shouldBe Quaternion(13, 3, 5, 7)
            11 + q shouldBe Quaternion(13, 3, 5, 7)
        }

        it should "add 11 + 13i" in {
            val i = Complex(11, 13)
            (q + i) shouldBe (Quaternion(13, 16, 5, 7))
            (i + q) shouldBe (q + i)
        }

        it should "add 11 + 13i + 17j + 19k" in {
            val q2 = Quaternion(11, 13, 17, 19)
            q + q2 shouldBe (Quaternion(13, 16, 22, 26))
        }

        it should "multiply by 11" in {
            q * 11 shouldBe Quaternion(22, 33, 55, 77)
        }

        it should "multiply by 13i" in {
            val q2 = Quaternion(0, 13, 0, 0)
            q * q2 shouldBe Quaternion(-39, 26, 91, -65)
            q2 * q shouldBe Quaternion(-39, 26, -91, 65)
        }

        it should "multiply by 17j" in {
            val q2 = Quaternion(0, 0, 17, 0)
            q * q2 shouldBe Quaternion(-85, -119, 34, 51)
            q2 * q shouldBe Quaternion(-85, 119, 34, -51)
        }

        it should "multiply by 19k" in {
            val q2 = Quaternion(0, 0, 0, 19)
            q * q2 shouldBe Quaternion(-133, 95, -57, 38)
        }

        it should "multiply by 11 + 13i" in {
            val q2 = Quaternion(11, 13, 0, 0)
            q * q2 shouldBe Quaternion(-17, 59, 146, 12)
            q2 * q shouldBe Quaternion(-17, 59, -36, 142)
        }

        it should "multiply by 11 + 13i + 17j + 19k" in {
            val q2 = Quaternion(11, 13, 17, 19)
            q * q2 shouldBe Quaternion(-235, 35, 123, 101)
            q2 * q shouldBe Quaternion(-235, 83, 55, 129)
        }

    }

    "Real numbers" should "convert" in {
        Quaternion(2) shouldBe Quaternion(2, 0, 0, 0)
    }

    "Complex numbers" should "convert" in {
        Quaternion(Complex(0, 3)) shouldBe Quaternion(0, 3, 0, 0)
        Quaternion(Complex(2, 3)) shouldBe Quaternion(2, 3, 0, 0)
    }

}
