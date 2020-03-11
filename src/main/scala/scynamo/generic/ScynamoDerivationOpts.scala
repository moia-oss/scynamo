package scynamo.generic

sealed abstract class ScynamoDerivationOpts[A] private (val transform: String => String)

object ScynamoDerivationOpts {
  def apply[A](transform: String => String = identity): ScynamoDerivationOpts[A] = new ScynamoDerivationOpts[A](transform) {}

  def default[A]: ScynamoDerivationOpts[A] = apply[A]()

  def prefix[A](prefix: String): ScynamoDerivationOpts[A] = apply[A](transform = s => s"$prefix$s")

  def suffix[A](suffix: String): ScynamoDerivationOpts[A] = apply[A](transform = s => s"$s$suffix")
}
