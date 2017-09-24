package org.validoc.rpgKata

import java.util.concurrent.atomic.AtomicInteger

case class Faction(name: String)

case class Level(l: Int) {
  def isFiveOrMoreHigher(other: Level) = l - 5 >= other.l
}

case class Character(name: String, faction: Faction, range: Range = Melee, level: Level = Level(1), alive: LiveStatus = Alive, hitPoints: HitPoints = HitPoints(1000)) {

  def damage(attacker: Character, distance: Meters, hitPoints: HitPoints): Character = ???
}


object Character {
  val damageCount = new AtomicInteger()

}

object Scenario extends App {
  val someAttacker = Character("Attacker", Faction("NPCs"))
  val thrud = Character("thrud", Faction("Berserkers"))

  println(thrud.damage(someAttacker, Meters(1), HitPoints(100)))
  //  println(s"Damage was called ${Character.damageCount.get} times")

}