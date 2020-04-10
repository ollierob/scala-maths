package net.ollie.utils

/**
 * Created by Ollie on 06/02/14.
 */
trait HomogeneousTuple[+T]
        extends Product {

    override def productIterator: Iterator[T] = iterator

    def iterator: Iterator[T]

    def size: Int

    override def canEqual(that: Any) = that.isInstanceOf[HomogeneousTuple[T]]

}

object Homogeneous1Tuple {

    implicit def apply[T](tuple: Tuple1[T]) = new Homogeneous1Tuple(tuple._1)

}

class Homogeneous1Tuple[T](override val _1: T)
        //extends Tuple1(a)
        extends Product1[T]
        with HomogeneousTuple[T] {

    final def size = 1

    def iterator = new Iterator[T] {

        var n = false

        def hasNext = !n

        def next(): T = {
            if (n) throw new NoSuchElementException
            n = true
            _1
        }

        override def size = Homogeneous1Tuple.this.size

    }

}

object Homogeneous2Tuple {

    implicit def apply[T](tuple: (T, T)) = new Homogeneous2Tuple(tuple._1, tuple._2)

}

class Homogeneous2Tuple[+T](override val _1: T, override val _2: T)
        extends Product2[T, T]
        with HomogeneousTuple[T] {

    final def size = 2

    def iterator = new Iterator[T] {

        var index = 0;

        override def hasNext: Boolean = index < size

        override def next(): T = {
            index += 1
            index match {
                case 1 => _1
                case 2 => _2
                case _ => throw new NoSuchElementException
            }
        }

        override def size = Homogeneous2Tuple.this.size

    }

}

object Homogeneous3Tuple {

    implicit def apply[T](tuple: (T, T, T)) = new Homogeneous3Tuple(tuple._1, tuple._2, tuple._3)

}

class Homogeneous3Tuple[T](override val _1: T, override val _2: T, override val _3: T)
        extends Product3[T, T, T]
        with HomogeneousTuple[T] {

    final def size = 3

    def iterator = new Iterator[T] {

        var index = 0;

        override def hasNext: Boolean = index < size

        override def next(): T = {
            index += 1
            index match {
                case 1 => _1
                case 2 => _2
                case 3 => _3
                case _ => throw new NoSuchElementException
            }
        }

        override def size = Homogeneous3Tuple.this.size

    }

}

object Homogeneous4Tuple {

    implicit def apply[T](tuple: (T, T, T, T)) = new Homogeneous4Tuple(tuple._1, tuple._2, tuple._3, tuple._4)

}

class Homogeneous4Tuple[T](override val _1: T, override val _2: T, override val _3: T, override val _4: T)
        extends Product4[T, T, T, T]
        with HomogeneousTuple[T] {

    final def size = 4

    def iterator = new Iterator[T] {

        var index = 0;

        override def hasNext: Boolean = index < size

        override def next(): T = {
            index += 1
            index match {
                case 1 => _1
                case 2 => _2
                case 3 => _3
                case 4 => _4
                case _ => throw new NoSuchElementException
            }
        }

        override def size = Homogeneous4Tuple.this.size

    }

}