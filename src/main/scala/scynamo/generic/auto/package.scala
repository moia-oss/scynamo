package scynamo.generic

package object auto {
  type AutoDerivationUnlocked[_] = AutoDerivationUnlocker
  implicit val autoDerivationUnlocker: AutoDerivationUnlocker = new AutoDerivationUnlocker
}

private[scynamo] class AutoDerivationUnlocker
