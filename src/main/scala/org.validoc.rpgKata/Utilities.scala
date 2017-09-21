package org.validoc.rpgKata

import scala.concurrent.{ExecutionContext, Future}

object Utilities {

implicit class AnyPimper[T](t: T){
  def withNonFunctional[T1](fn: T => T1) = fn(t)
}
  implicit class FunctionPimper[T, T1](fn: T => T1) {
    def ~>[T2](nextFn: T1 => T2) = fn andThen nextFn
    def toFutureFn = { t: T => Future.successful(fn(t)) }
  }

  implicit class KleisliPimper[T, T1](fn: T => Future[T1]) {
    def ~>[T2](nextFn: T1 => T2)(implicit ex: ExecutionContext): (T => Future[T2]) = { t: T => fn(t).map(nextFn) }

    def ~~>[T2](nextFn: T1 => Future[T2])(implicit ex: ExecutionContext): (T => Future[T2]) = { t: T => fn(t).flatMap(nextFn) }

    def toFutureFn = { t: T => Future.successful(fn(t)) }
  }

  implicit class BooleanFunctionPimper[T](fn: T => Boolean) {
    def ifTrue(guardedFunction: T => T) = { t: T => if (fn(t)) guardedFunction(t) else t }
  }

}
