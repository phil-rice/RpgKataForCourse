package org.validoc.rpgKata
import Utilities._

class UtilitiesSpec extends KataSpec {

  val guardFn: (String => Boolean) = { s: String => s == "true" }
  val guardedFn: (String => String) = { s: String => s + "executed" }
   val madeFn = guardFn ifTrue(guardedFn)

  behavior of "BooleanFunctionPimper"

  it should "return a function that will executed the guarded function if the guard function is true" in {
      madeFn("true") shouldBe "trueexecuted"
  }

  it should "return a function that will return the parameter if the guard function is false" in {
    madeFn("other") shouldBe "other"

  }

}
