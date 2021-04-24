package mode
import key.MajorKey
import util.ConstructorUtils

/**
  * Represents the Phrygian music mode
  */
case class Phrygian(tonic: String) extends Mode {
  override protected val tonicIndex: Int = 2

  override protected implicit val majorKey: MajorKey =
    MajorKey(ascending(5)).get
}

object Phrygian {
  def apply(tonic: String): Option[Phrygian] =
    ConstructorUtils.validTonicConstructor(tonic, s => new Phrygian(s))
}
