package org.validoc.rpgKata

case class ChallengeRating(rating: Int)

case class MonsterHitPoints(hp: Int)

case class Monster(description: String, meleeMonster: Boolean, challengeRating: ChallengeRating, hitPoints: MonsterHitPoints)

object Monster {
  val monsterFaction = Faction("Monster")


  implicit object WithinRangeForMonster extends WithinRange[Monster] {
    def monsterToRange(m: Monster) = if (m.meleeMonster) Melee else Ranged

    override def apply(v1: Monster, v2: Meters) = monsterToRange(v1).canHit(v2)
  }

  implicit object HasLevelForMonster extends HasLevel[Monster] {
    override def apply(v1: Monster) = Level(v1.challengeRating.rating * 2)
  }

  implicit object HasFactionForMonster extends HasFaction[Monster] {
    override def apply(v1: Monster) = monsterFaction
  }

  implicit object KillForMonster extends Kill[Monster] {
    override def apply(v1: Monster) = v1.copy(hitPoints = MonsterHitPoints(0))
  }

  implicit val hitPointsL = new Lens[Monster, HitPoints](m => HitPoints(m.hitPoints.hp), hp => c => c.copy(hitPoints = MonsterHitPoints(hp.hp)))

}