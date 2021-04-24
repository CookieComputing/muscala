package mode
import key.MajorKey
import util.ConstructorUtils

/**
  * Represents a mixolydian mode.
  */
case class Mixolydian(tonic: String) extends Mode {
  override protected val tonicIndex: Int = 4

  override protected implicit val majorKey: MajorKey =
    MajorKey(ascending(3)).get
}

object Mixolydian {
  def apply(tonic: String): Option[Mixolydian] =
    ConstructorUtils.validTonicConstructor(tonic, s => new Mixolydian(s))
}
