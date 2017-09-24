package org.validoc.rpgKata

case class HitPoints(hp: Int) {
  def moreThanZero = hp > 0

  def +(that: HitPoints): HitPoints = HitPoints(hp + that.hp)

  def -(that: HitPoints): HitPoints = HitPoints(hp - that.hp)

  def lessThanZero: Boolean = hp < 0

  def moreThanMax: Boolean = hp > 1000

  def fiftyPercent = HitPoints(hp / 2)

  def plusFiftyPercent = HitPoints(hp * 3 / 2)

  def asIs = this
}
