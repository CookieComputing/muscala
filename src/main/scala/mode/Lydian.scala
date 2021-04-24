package mode
import key.MajorKey
import util.ConstructorUtils

/**
  * Represents the lydian mode.
  */
case class Lydian private (tonic: String) extends Mode {
  override protected val tonicIndex: Int = 3

  override protected implicit val majorKey: MajorKey =
    MajorKey(ascending(4)).get
}

object Lydian {
  def apply(tonic: String): Option[Lydian] =
    ConstructorUtils.validTonicConstructor(tonic, s => new Lydian(s))
}
