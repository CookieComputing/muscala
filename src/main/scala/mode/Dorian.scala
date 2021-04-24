package mode
import key.MajorKey
import util.ConstructorUtils

/**
  * Represents the Dorian mode.
  */
case class Dorian(tonic: String) extends Mode {
  override protected val tonicIndex: Int = 1
  override protected implicit val majorKey: MajorKey =
    MajorKey(ascending(6)).get
}

object Dorian {
  def apply(tonic: String): Option[Dorian] =
    ConstructorUtils.validTonicConstructor(tonic, s => new Dorian(s))
}
