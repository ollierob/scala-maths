package net.ollie.utils

/**
 * Created by Ollie on 06/02/14.
 */
trait HomogeneousTuple[+T]
        extends Product {

    override def productIterator: Iterator[T] = iterator

    def iterator: Iterator[T]

    def size: Int

}

object Homogeneous1Tuple {

    implicit def apply[T](tuple: Tuple1[T]) = new Homogeneous1Tuple(tuple._1)

}

class Homogeneous1Tuple[T](a: T)
        extends Tuple1(a)
        with HomogeneousTuple[T] {

    final def size = 1

    def iterator = new Iterator[T] {

        var n = false

        def hasNext = !n

        def next(): T = {
            if (n) throw new NoSuchElementException
            n = true
            a
        }

    }

}

object Homogeneous2Tuple {

    implicit def apply[T](tuple: (T, T)) = new Homogeneous2Tuple(tuple._1, tuple._2)

}

class Homogeneous2Tuple[+T](a: T, b: T)
        extends Tuple2(a, b)
        with HomogeneousTuple[T] {

    final def size = 2

    def iterator = new Iterator[T] {

        var index = 0;

        override def hasNext: Boolean = index < 3

        override def next(): T = {
            index += 1
            index match {
                case 1 => a
                case 2 => b
                case _ => throw new NoSuchElementException
            }
        }

    }

}

object Homogeneous3Tuple {

    implicit def apply[T](tuple: (T, T, T)) = new Homogeneous3Tuple(tuple._1, tuple._2, tuple._3)

}

class Homogeneous3Tuple[T](a: T, b: T, c: T)
        extends Tuple3(a, b, c)
        with HomogeneousTuple[T] {

    final def size = 3

    def iterator = new Iterator[T] {

        var index = 0;

        override def hasNext: Boolean = index < 3

        override def next(): T = {
            index += 1
            index match {
                case 1 => a
                case 2 => b
                case 3 => c
                case _ => throw new NoSuchElementException
            }
        }

    }

}