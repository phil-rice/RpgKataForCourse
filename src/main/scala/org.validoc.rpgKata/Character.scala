package org.validoc.rpgKata

import java.util.concurrent.atomic.AtomicInteger
import Utilities._
import AttackError._

case class Faction(name: String)

case class Level(l: Int) {
  def isFiveOrMoreHigher(other: Level) = l - 5 >= other.l

  def -(other: Level) = LevelDifference(l - other.l)
}

case class LevelDifference(diff: Int) {
  def fold[To](lowLevel: => To, normal: => To, highLevel: => To) =
    if (diff > 5) highLevel else if (diff < -5) lowLevel else normal

  def fold2[Mid, To](mid: Mid, lowLevel: Mid => To, normal: Mid => To, highLevel: Mid => To): To =
    fold[To](lowLevel(mid), normal(mid), highLevel(mid))
}

object AttackError {

  implicit class OptionPimper(boolean: Boolean) {
    def ifFalse[Att, Def](error: AttackError) = if (boolean) None else Some(error)

    def ifTrue[Att, Def](error: AttackError) = if (boolean) Some(error) else None
  }

}

sealed trait AttackError

case object AttackerIsDefender extends AttackError

case object AttackerNotWithinRange extends AttackError

case object AttackerSameFaction extends AttackError

case object DamageCannotBeLessThanZero extends AttackError


case class Attack[Att, Def](attacker: Att, defender: Def, hitPoints: HitPoints, distance: Meters)(
  implicit withinRange: WithinRange[Att], findLevelDiff: FindLevelDiff[Att, Def], sameFaction: SameFaction[Att, Def], hitPointLens: Lens[Def, HitPoints], killDef: Kill[Def]) {

  val inRangeCheck = withinRange(attacker, distance).ifFalse(AttackerNotWithinRange)
  val sameFactionCheck = sameFaction(attacker, defender).ifTrue(AttackerSameFaction)
  val damageNotNegativeCheck = hitPoints.lessThanZero.ifTrue(DamageCannotBeLessThanZero)
  val attackIsDefenderCheck = (attacker == defender).ifTrue(AttackerIsDefender)
  val errors = List(inRangeCheck, sameFactionCheck, damageNotNegativeCheck, attackIsDefenderCheck).flatten
  val realDamage = findLevelDiff(attacker, defender).fold2[HitPoints, HitPoints](hitPoints, lowLevel = _.fiftyPercent, normal = _.asIs, highLevel = _.plusFiftyPercent)
  val killIfNegativeHitPoints: Def => Def = (hitPointLens.has(_.lessThanZero) ifTrue killDef)

  def damage = errors or (defender |> hitPointLens.transform(_ - realDamage) ~> printit("after damage applied {0}") ~> killIfNegativeHitPoints)
}

object Character {
  val damageCount = new AtomicInteger()

  implicit object WithinRangeForCharacter extends WithinRange[Character] {
    override def apply(v1: Character, v2: Meters) = v1.range.canHit(v2)
  }

  implicit object HasLevelForCharacter extends HasLevel[Character] {
    override def apply(v1: Character) = v1.level
  }

  implicit object HasFactionForCharacter extends HasFaction[Character] {
    override def apply(v1: Character) = v1.faction
  }

  implicit object KillForCharacter extends Kill[Character] {
    override def apply(v1: Character) = v1.copy(alive = Dead, hitPoints = HitPoints(0))
  }

  implicit val hitPointsL = new Lens[Character, HitPoints](_.hitPoints, hp => c => c.copy(hitPoints = hp))

}

case class Character(name: String, faction: Faction, range: Range = Melee, level: Level = Level(1), alive: LiveStatus = Alive, hitPoints: HitPoints = HitPoints(1000)) {
  def damage(attacker: Character, distance: Meters, hitPoints: HitPoints) = new Attack[Character, Character](attacker, this, hitPoints, distance).damage
}


object Scenario extends App {
  val someAttacker = Character("Attacker", Faction("NPCs"))
  val thrud = Character("thrud", Faction("Berserkers"))

  println(thrud.damage(someAttacker, Meters(1), HitPoints(100)))
  //  println(s"Damage was called ${Character.damageCount.get} times")

}