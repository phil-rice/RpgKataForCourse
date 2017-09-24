package org.validoc.rpgKata

import java.text.MessageFormat

import scala.concurrent.{ExecutionContext, Future}

object Utilities {

  def printit[T](msg: String) = { t: T =>
    println(MessageFormat.format(msg, t.toString))
    t
  }

  implicit class AnyPimper[T](t: T) {
    def withNonFunctional[T1](fn: T => T1) = fn(t)

    def |>[T1](fn: T => T1) = fn(t)
  }

  implicit class FunctionPimper[T, T1](val fn: T => T1) extends AnyVal {
    def ~>[T2](nextFn: T1 => T2) = fn andThen nextFn
    def |+|[T2](nextFn: T1 => T2) = fn andThen nextFn

    def decide[T2](guardFn: T1 => Boolean, ifTrue: T1 => T2, ifFalse: T1 => T2): (T) => T2 = { t: T =>
      val t1 = fn(t)
      if (guardFn(t1)) ifTrue(t1) else ifFalse(t1)
    }


    def toFutureFn = { t: T => Future.successful(fn(t)) }
  }


  implicit class KleisliPimper[T, T1](fn: T => Future[T1]) {
    def ~>[T2](nextFn: T1 => T2)(implicit ex: ExecutionContext): (T => Future[T2]) = { t: T => fn(t).map(nextFn) }

    def ~~>[T2](nextFn: T1 => Future[T2])(implicit ex: ExecutionContext): (T => Future[T2]) = { t: T => fn(t).flatMap(nextFn) }

    def toFutureFn = { t: T => Future.successful(fn(t)) }
  }


  implicit class BooleanFunctionPimper[T](fn: T => Boolean) {
    def ifTrue(guardedFunction: T => T) = { t: T => if (fn(t)) guardedFunction(t) else t }

    def ifFalse(guardedFunction: T => T) = { t: T =>
      println(s"In ifFalse $t => ${fn(t)}")
      if (fn(t)) t else guardedFunction(t)
    }

    def fold[T1](falseFn: T => T1, guardedFunction: T => T1) = { t: T => if (fn(t)) guardedFunction(t) else falseFn(t) }

    //    def ifFalse[T1](falseFn: T => T1) = (fn, falseFn)
    //
    //    def ifFalse[T1](lens: Lens[T, T1]) = (fn, lens.get)

    def &&(otherFunction: T => Boolean) = { t: T => fn(t) && otherFunction(t) }
  }


  implicit class ListPimper[T](list: List[T]) {
    def or[T1](value: => T1) = {
      if (list.size > 0) Left(list) else Right(value)
    }
  }

}