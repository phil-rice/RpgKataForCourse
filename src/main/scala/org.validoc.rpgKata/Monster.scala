package org.validoc.rpgKata

//Pretending this is in another jar
case class ChallengeRating(rating: Int)

case class MonsterHitPoints(hp: Int)

case class Monster(description: String, meleeMonster: Boolean, challengeRating: ChallengeRating, hitPoints: MonsterHitPoints)

//End of pretending this is in another jar

trait CanFight[T] {
  def range(t: T): Range

  def faction(t: T): Faction

  def level(t: T): Level

  def kill(t: T): T

  def hitPoints(t: T): HitPoints

  def setHitPoints(t: T, hp: HitPoints): T
}

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

  implicit class CanFightPimper[T](t: T)(implicit canFight: CanFight[T]) {
    def range = canFight.range(t)

    def faction = canFight.faction(t)

    def level = canFight.level(t)

    def kill = canFight.kill(t)

    def setHitPoints(hp: HitPoints) = canFight.setHitPoints(t, hp)

    def hitPoints = canFight.hitPoints(t)
  }


  def someMethod(someargs: Int) = {
    "someresult"
  }

  val someFunction = {someargs: Int => "someresult"}




  val m1 = Monster("Big Dragon", true, ChallengeRating(7), MonsterHitPoints(10000))
  println(m1.range)

  implicit class PimpAny[T](t: T)(implicit canFight: CanFight[T]) {
    def dump(msg: String) = println(msg + ": " + t + " range: " + canFight.range(t))
  }

  m1.dump("First monster")
  m1.faction


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