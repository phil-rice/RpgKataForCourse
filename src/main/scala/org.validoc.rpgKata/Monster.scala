package org.validoc.rpgKata

case class ChallengeRating(rating: Int)

case class MonsterHitPoints(hp: Int)

case class Monster(description: String, meleeMonster: Boolean, challengeRating: ChallengeRating, hitPoints: MonsterHitPoints)

object Monster {
  val monsterFaction = Faction("Monster")

  implicit object CanFightForMonster extends CanFight[Monster] {
    override def range(t: Monster) = if (t.meleeMonster) Melee else Ranged

    override def faction(t: Monster) = monsterFaction

    override def level(t: Monster) = Level(t.challengeRating.rating * 2 - 2)

    override def kill(t: Monster) = t.copy(hitPoints = MonsterHitPoints(0))

    override def setHitPoints(t: Monster, hitPoints: HitPoints) = t.copy(hitPoints = MonsterHitPoints(hitPoints.hp))

    override def hitPoints(t: Monster) = HitPoints(t.hitPoints.hp)
  }

}