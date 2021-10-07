package scynamo.generic

package object auto {
  type AutoDerivationUnlocked[_] = AutoDerivationUnlocker
  implicit val unlocker: AutoDerivationUnlocker = new AutoDerivationUnlocker {}
}

sealed abstract private[scynamo] class AutoDerivationUnlocker
