package org.validoc.rpgKata

import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

trait KataSpec extends FlatSpec with Matchers with MockFactory

object CharacterFixture {

  import HitPointsFixture._

  val berserkers = Faction("Berserker")
  val monsters = Faction("Monster")
  val thrud = Character("Thrud", berserkers)

  def thrudWith(hitPoints: Int) = thrud.copy(hitPoints = HitPoints(hitPoints))

  val deadThrud = thrudWith(0).copy(alive = Dead)
  val thrud50HitPoints = thrudWith(50)
  val thrud100HitPoints = thrudWith(100)
  val thrud150HitPoints = thrudWith(150)
  val thrud200HitPoints = thrudWith(200)
  val thrud900HitPoints = thrudWith(900)
  val thrud0HitPoints = thrudWith(0)
  val thrudMinus1HitPoints = thrudWith(-1)
  val thrudMinus100HitPoints = thrudWith(-100)

  val someMeleeMonster = Character("SomeAttacker", monsters, Melee)
  val someRangedMonster = Character("SomeAttacker", monsters, Ranged)
  val someMeleeBerserker = Character("SomeAttacker", berserkers, Melee)
  val someRangedBerserker = Character("SomeAttacker", berserkers, Ranged)
}

class CharacterSpec extends KataSpec {

  import CharacterFixture._
  import HitPointsFixture._

  val close = Meters(1)
  val maxMelee = Meters(2)
  val justOutsideMelee = Meters(3)
  val maxRanged = Meters(20)
  val outsideRanged = Meters(21)

  implicit class ResultPimpler(l: Either[List[AttackError], Character]) {
    def succeeded = l.right.getOrElse(throw new RuntimeException(s"Expected it to succeed. Was actually ${l.left.get}"))

    def failed = l.left.getOrElse(throw new RuntimeException(s"Expected it to fail. Was actually ${l.right.get}"))
  }

  behavior of "Character"

  it should "Start with 1000 hitpoints by default" in {
    thrud.hitPoints shouldBe HitPoints(1000)
  }

  it should "start alive by default" in {
    thrud.alive shouldBe Alive
  }

  behavior of "receiving negative damage"

  it should "ignore -ve damage" in {
    thrud200HitPoints.damage(someMeleeMonster, close, hpMinus100).failed shouldBe List(DamageCannotBeLessThanZero)
  }

  behavior of "Death"

  it should "die if damage received takes it negative" in {
    thrud100HitPoints.damage(someMeleeMonster, close, hp900).succeeded shouldBe deadThrud
  }

  behavior of "Melee Attacks"

  it should "only received damage when close" in {
    thrud.damage(someMeleeMonster, close, hp100).succeeded shouldBe thrud.copy(hitPoints = hp900)
    thrud.damage(someMeleeMonster, maxMelee, hp100).succeeded shouldBe thrud.copy(hitPoints = hp900)
  }
  it should "ignore damage when outside melee range" in {
    thrud.damage(someMeleeMonster, justOutsideMelee, hp100).failed shouldBe List(AttackerNotWithinRange)
    thrud.damage(someMeleeMonster, maxRanged, hp100).failed shouldBe List(AttackerNotWithinRange)
  }

  behavior of "Not Damaging self"

  it should "not be possible to hurt yourself" in {
    thrud.damage(thrud, close, hp100).failed shouldBe List(AttackerSameFaction, AttackerIsDefender)
  }

  behavior of "Ranged Attacks"

  it should "only received damage when close" in {
    thrud.damage(someRangedMonster, close, hp100).succeeded shouldBe thrud.copy(hitPoints = hp900)
    thrud.damage(someRangedMonster, maxMelee, hp100).succeeded shouldBe thrud.copy(hitPoints = hp900)
    thrud.damage(someRangedMonster, justOutsideMelee, hp100).succeeded shouldBe thrud.copy(hitPoints = hp900)
    thrud.damage(someRangedMonster, maxRanged, hp100).succeeded shouldBe thrud.copy(hitPoints = hp900)
  }

  it should "ignore damage when outside  range" in {
    thrud.damage(someRangedMonster, outsideRanged, hp100).failed shouldBe List(AttackerNotWithinRange)
  }

  behavior of "Factions"

  it should "be possible to hurt another faction" in {
    thrud.damage(someMeleeMonster, close, hp100).succeeded shouldBe thrud.copy(hitPoints = hp900)

  }
  it should "not be possible to hurt another faction" in {
    thrud.damage(someMeleeBerserker, close, hp100).failed shouldBe List(AttackerSameFaction)
  }

}

