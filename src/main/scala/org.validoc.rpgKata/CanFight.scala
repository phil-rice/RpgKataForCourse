package org.validoc.rpgKata


trait WithinRange[T] extends ((T, Meters) => Boolean)

trait SameFaction[T1, T2] extends ((T1, T2) => Boolean)


object SameFaction {
  implicit def SameFactionFromHasFaction[T1, T2](implicit hasFaction1: HasFaction[T1], hasFaction2: HasFaction[T2]) = new SameFaction[T1, T2] {
    override def apply(v1: T1, v2: T2) = hasFaction1(v1) == hasFaction2(v2)
  }
}

trait FindLevelDiff[T1, T2] extends ((T1, T2) => LevelDifference)

object FindLevelDiff {
  implicit def findLevelDiff[T1, T2](implicit hasLevel1: HasLevel[T1], hasLevel2: HasLevel[T2]) = new FindLevelDiff[T1, T2] {
    override def apply(v1: T1, v2: T2) = hasLevel1(v1) - hasLevel2(v2)
  }
}

trait HasFaction[T] extends (T => Faction)

trait HasLevel[T] extends (T => Level)


trait Kill[T] extends (T => T)


