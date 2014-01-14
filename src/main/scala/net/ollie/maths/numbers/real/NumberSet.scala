package net.ollie.maths.numbers.real

/**
 * Created by Ollie on 07/01/14.
 */
trait NumberSet {

    def unary_-(): NumberSet

    def isEmpty: Boolean

}

object NumberSet {

    def apply(): NumberSet = EmptyNumberSet

}

object EmptyNumberSet extends NumberSet {

    def unary_-() = this

    def isEmpty = true

}