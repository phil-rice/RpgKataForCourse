package org.validoc.rpgKata

class LevelSpec extends KataSpec {

  behavior of "Level"

  it should "allow subtraction of two levels" in {
    Level(10) - Level(4) shouldBe LevelDifference(6)
    Level(4) - Level(10) shouldBe LevelDifference(-6)
    Level(4) - Level(4) shouldBe LevelDifference(0)
  }

  behavior of "LevelDifference"

  it should "allow a fold for 'normal range'" in {
    LevelDifference(-5).fold2[String, String]("something", _ => "Low", _ => "normal", _ => "high") shouldBe "normal"
    LevelDifference(-3).fold2[String, String]("something", _ => "Low", _ => "normal", _ => "high") shouldBe "normal"
    LevelDifference(0).fold2[String, String]("something", _ => "Low", _ => "normal", _ => "high") shouldBe "normal"
    LevelDifference(3).fold2[String, String]("something", _ => "Low", _ => "normal", _ => "high") shouldBe "normal"
    LevelDifference(5).fold2[String, String]("something", _ => "Low", _ => "normal", _ => "high") shouldBe "normal"
  }
  it should "allow a fold for 'low level' " in {
    LevelDifference(-6).fold2[String, String]("something", _ => "Low", _ => "normal", _ => "high") shouldBe "Low"
  }
  it should "allow a fold for 'high level' " in {
    LevelDifference(6).fold2[String, String]("something", _ => "Low", _ => "normal", _ => "high") shouldBe "high"
  }
}
