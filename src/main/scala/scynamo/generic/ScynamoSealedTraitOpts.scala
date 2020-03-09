package scynamo.generic

sealed abstract class ScynamoSealedTraitOpts[A] private (val discriminator: String, val transform: String => String)

object ScynamoSealedTraitOpts {
  val DEFAULT_DISCRIMINATOR = "_SCYNAMO_DEFAULT_DISCRIMINATOR_"

  def apply[A](
      discriminator: String = DEFAULT_DISCRIMINATOR,
      transform: String => String = identity
  ): ScynamoSealedTraitOpts[A] =
    new ScynamoSealedTraitOpts[A](discriminator, transform) {}

  def default[A]: ScynamoSealedTraitOpts[A] = apply[A]()

  def prefix[A](prefix: String): ScynamoSealedTraitOpts[A] = apply[A](transform = s => s"$prefix$s")

  def suffix[A](suffix: String): ScynamoSealedTraitOpts[A] = apply[A](transform = s => s"$s$suffix")

  def discrimator[A](discrimator: String): ScynamoSealedTraitOpts[A] = apply[A](discriminator = discrimator)
}
