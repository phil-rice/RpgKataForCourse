package org.validoc.rpgKata

trait CanFight[T] {
  def range(t: T): Range

  def faction(t: T): Faction

  def level(t: T): Level

  def kill(t: T): T

  def hitPoints(t: T): HitPoints
  def setHitPoints(t: T, hitPoints: HitPoints): T
}

object CanFight {

  implicit class CanFightPimper[T](t: T)(implicit canFight: CanFight[T]) {
    def range = canFight.range(t)

    def faction = canFight.faction(t)

    def level = canFight.level(t)

    def kill = canFight.kill(t)

    def setHitPoints(hitPoints: HitPoints) = canFight.setHitPoints(t, hitPoints)
    def hitPoints = canFight.hitPoints(t)
  }

}