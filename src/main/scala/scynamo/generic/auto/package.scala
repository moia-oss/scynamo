package scynamo.generic

package object auto {
  type AutoDerivationUnlocked[_] = AutoDerivationUnlocker
  implicit val unlocker: AutoDerivationUnlocker = new AutoDerivationUnlocker {}
}

private[scynamo] sealed abstract class AutoDerivationUnlocker
