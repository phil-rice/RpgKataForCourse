package org.validoc.rpgKata

case class Meters(m: Int) {
  def <=(other: Meters) = m <= other.m
}

sealed abstract class Range(meters: Meters) {
  def canHit(distance: Meters): Boolean = distance <= meters
}

case object Melee extends Range(Meters(2))

case object Ranged extends Range(Meters(20))