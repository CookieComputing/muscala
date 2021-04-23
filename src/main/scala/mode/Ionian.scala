package mode
import key.MajorKey
import util.ConstructorUtils

/**
  * Represents the Ionian mode, which is effectively the major scale.
  */
case class Ionian(tonic: String) extends Mode {
  override protected val tonicIndex: Int = 0

  override protected implicit val majorKey: MajorKey = MajorKey(tonic).get
}

object Ionian {
  def apply(tonic: String): Option[Ionian] =
    ConstructorUtils.validTonicConstructor(tonic, s => new Ionian(s))
}
