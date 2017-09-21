package org.validoc.rpgKata

object HitPointsFixture{
  val hp1001 = HitPoints(1001)
  val hp1000 = HitPoints(1000)
  val hp900 = HitPoints(900)
  val hp100 = HitPoints(100)
  val hp200 = HitPoints(200)
  val hp0 = HitPoints(0)
  val hpMinus1 = HitPoints(-1)
  val hpMinus100 = HitPoints(-100)

}

class HitPointsSpec extends KataSpec  {

  import HitPointsFixture._

  behavior of "HitPoints"

  it should "Allow hitpoints to be subtracted" in {
    hp1000-hp100 shouldBe hp900
    hp1000-hp900 shouldBe hp100
  }

  it should "Allow hitpoints to be added" in {
    hp900+hp100 shouldBe hp1000
    hp100+hp900 shouldBe hp1000
  }

  it should "implement to be less than zero" in {
    hp100.lessThanZero shouldBe false
    hp0.lessThanZero shouldBe false
    hpMinus1.lessThanZero shouldBe true
    hpMinus100.lessThanZero shouldBe true
  }
  it should "implement more than max" in {
    hp100.moreThanMax shouldBe false
    hp0.moreThanMax shouldBe false
    hpMinus1.moreThanMax shouldBe false
    hpMinus100.moreThanMax shouldBe false
    hp1001.moreThanMax shouldBe true
  }
}
