package net.ollie.utils

/**
 * Created by Ollie on 22/02/14.
 */
trait OptionalBigDecimal {

    def isDefined: Boolean

    def get: BigDecimal

    def getOrElse(default: => BigDecimal): BigDecimal = if (isDefined) this.get else default

}

object OptionalBigDecimal {

    def none: OptionalBigDecimal = None

    def some(d: BigDecimal): OptionalBigDecimal = new SomeBigDecimal(d)

    implicit def apply(option: Option[BigDecimal]): OptionalBigDecimal = option match {
        case Some(d) => some(d)
        case _ => none
    }

    implicit def apply(some: Some[BigDecimal]): SomeBigDecimal = new SomeBigDecimal(some.get)

}

final case class SomeBigDecimal(d: BigDecimal)
        extends AnyRef
        with OptionalBigDecimal {

    def isDefined = true

    def get = d

}

object None
        extends OptionalBigDecimal {

    def isDefined = false

    def get = ???

}