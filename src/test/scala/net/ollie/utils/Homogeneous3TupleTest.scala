package net.ollie.utils

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by Ollie on 09/02/14.
 */
class Homogeneous3TupleTest extends FlatSpec with Matchers {

    behavior of "(object, object, object)"

    val o1 = new Object
    val o2 = new Object
    val o3 = new Object
    val tuple: Homogeneous3Tuple[Any] = (o1, o2, o3)

    it should "preserve order" in {
        tuple._1 shouldBe o1
        tuple._2 shouldBe o2
        tuple._3 shouldBe o3
    }

    it should "iterate" in {
        val iterator = tuple.iterator
        iterator.next shouldBe o1
        iterator.next shouldBe o2
        iterator.next shouldBe o3
        iterator.hasNext shouldBe false
    }

}
