package org.validoc.rpgKata

import java.util.concurrent.atomic.AtomicInteger

case class HitPoints(hp: Int) {
  def +(that: HitPoints): HitPoints = HitPoints(hp + that.hp)

  def -(that: HitPoints): HitPoints = HitPoints(hp - that.hp)

  def lessThanZero: Boolean = hp < 0

  def moreThanMax: Boolean = hp > 1000
}

case class Level(l: Int)

trait LiveStatus

case object Alive extends LiveStatus

case object Dead extends LiveStatus

case class Character(name: String, alive: LiveStatus = Alive, hitPoints: HitPoints = HitPoints(1000)) {
  def damage(hitPoints: HitPoints): Character = ???
}

object Character {
  val damageCount = new AtomicInteger()
}

object Scenario extends App {
  val thrud = Character("thrud")

  println(thrud.damage(HitPoints(100)))
  println(s"Damage was called ${Character.damageCount.get} times")

}