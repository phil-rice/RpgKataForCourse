package org.validoc.rpgKata

import scala.concurrent.{ExecutionContext, Future}

object Utilities {

  implicit class AnyPimper[T](t: T) {
    def withNonFunctional[T1](fn: T => T1) = fn(t)

    def |>[T1](fn: T => T1) = fn(t)
  }

  implicit class FunctionPimper[T, T1](val fn: T => T1) extends AnyVal {
    def ~>[T2](nextFn: T1 => T2) = fn andThen nextFn

    //    def mergeWith[T2](fn2: T => T2): (T => T1, T => T2) = (fn, fn2)
    //
    //    def passTo[T2](passFn: (T1 => T => T2)): T => T2 = { from: T =>
    //      val to1: T1 = fn(from)
    //      passFn(to1)(from)
    //    }

    def toFutureFn = { t: T => Future.successful(fn(t)) }
  }

  //  implicit class LensPimper[T, T1](val lens: Lens[T, T1]) extends AnyVal {
  //    def mergeWith[T2](fn2: T => T2): (T => T1, T => T2) = lens.get.mergeWith(fn2)
  //  }
  //
  //  implicit class LensToBooleanPimper[T](val lens: Lens[T, Boolean]) extends AnyVal {
  //    def ifTrue(guardedFunction: T => T) = { t: T => if (lens.get(t)) guardedFunction(t) else t }
  //  }

  //  implicit class TupleFunctionPimpler[From, To1, To2](tuple: (From => To1, From => To2)) {
  //    def using[To](fn: To1 => To2 => To) = { from: From =>
  //      val to1 = tuple._1(from)
  //      val to2 = tuple._2(from)
  //      fn(to1)(to2)
  //    }
  //  }
  //
  //  def split[From, To1, To2](fn1: From => To1, fn2: From => To2) = (fn1, fn2)
  //
  //  def withInput[From, To1, To](fn1: From => To1, merge: To1 => From => To): From => To = { from: From =>
  //    val to1: To1 = fn1(from)
  //    merge(to1)(from)
  //  }

  implicit class KleisliPimper[T, T1](fn: T => Future[T1]) {
    def ~>[T2](nextFn: T1 => T2)(implicit ex: ExecutionContext): (T => Future[T2]) = { t: T => fn(t).map(nextFn) }

    def ~~>[T2](nextFn: T1 => Future[T2])(implicit ex: ExecutionContext): (T => Future[T2]) = { t: T => fn(t).flatMap(nextFn) }

    def toFutureFn = { t: T => Future.successful(fn(t)) }
  }

  //  implicit class TumpleOfFnAndBooleanPimper[From, To](tuple: (From => Boolean, From => To)) {
  //    def ifTrue(guardedFunction: From => To) = { f: From => if (tuple._1(f)) guardedFunction(f) else tuple._2(f) }
  //  }

  implicit class BooleanFunctionPimper[T](fn: T => Boolean) {
    def ifTrue(guardedFunction: T => T) = { t: T => if (fn(t)) guardedFunction(t) else t }

    def fold[T1](falseFn: T => T1, guardedFunction: T => T1) = { t: T => if (fn(t)) guardedFunction(t) else falseFn(t) }

    //    def ifFalse[T1](falseFn: T => T1) = (fn, falseFn)
    //
    //    def ifFalse[T1](lens: Lens[T, T1]) = (fn, lens.get)

    def &&(otherFunction: T => Boolean) = { t: T => fn(t) && otherFunction(t) }
  }


}