package org.validoc.rpgKata

import java.util.concurrent.atomic.AtomicInteger
import Utilities._

case class Faction(name: String)

case class Level(l: Int) {
  def isFiveOrMoreHigher(other: Level) = l - 5 >= other.l
}

case class Attack[Att: CanFight, Def: CanFight](attacker: Att, defender: Def, hitPoints: HitPoints, distance: Meters) {

  import CanFight._

  val realDamage = (attacker.level, defender.level) match {
    case (l1, l2) if l1 isFiveOrMoreHigher l2 => hitPoints.plusFiftyPercent
    case (l1, l2) if l2 isFiveOrMoreHigher l1 => hitPoints.fiftyPercent
    case _ => hitPoints.asIs
  }

  def damage: Def = {
    if ((attacker != this) && (attacker.range.canHit(distance)) && attacker.faction != defender.faction && !hitPoints.lessThanZero) {
      val newHitPoints = this.defender.hitPoints - realDamage
      if (newHitPoints.lessThanZero) defender.kill else defender.setHitPoints(newHitPoints)
    } else defender
  }
}

object Character {
  val damageCount = new AtomicInteger()

  implicit object CanFightForCharacter extends CanFight[Character] {
    override def range(t: Character) = t.range
    override def faction(t: Character) = t.faction
    override def level(t: Character) = t.level
    override def kill(t: Character) = t.copy(alive = Dead, hitPoints = HitPoints(0))
    override def setHitPoints(t: Character, hitPoints: HitPoints) = t.copy(hitPoints = hitPoints)
    override def hitPoints(t: Character) = t.hitPoints
  }

}

case class Character(name: String, faction: Faction, range: Range = Melee, level: Level = Level(1), alive: LiveStatus = Alive, hitPoints: HitPoints = HitPoints(1000)) {
  def damage(attacker: Character, distance: Meters, hitPoints: HitPoints): Character = new Attack[Character, Character](attacker, this, hitPoints, distance).damage
}


object Scenario extends App {
  val someAttacker = Character("Attacker", Faction("NPCs"))
  val thrud = Character("thrud", Faction("Berserkers"))

  println(thrud.damage(someAttacker, Meters(1), HitPoints(100)))
  //  println(s"Damage was called ${Character.damageCount.get} times")

}