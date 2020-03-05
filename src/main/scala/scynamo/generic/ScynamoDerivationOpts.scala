package scynamo.generic

sealed abstract class ScynamoDerivationOpts[A] private (val transform: String => String)

object ScynamoDerivationOpts {
  def default[A]: ScynamoDerivationOpts[A] = new ScynamoDerivationOpts[A](identity) {}

  def apply[A](f: String => String): ScynamoDerivationOpts[A] = new ScynamoDerivationOpts[A](f) {}

  def prefix[A](prefix: String): ScynamoDerivationOpts[A] = new ScynamoDerivationOpts[A](s => s"$prefix$s") {}

  def suffix[A](suffix: String): ScynamoDerivationOpts[A] = new ScynamoDerivationOpts[A](s => s"$s$suffix") {}
}
