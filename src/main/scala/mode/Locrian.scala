package mode
import key.MajorKey
import util.ConstructorUtils

case class Locrian(tonic: String) extends Mode {
  override protected val tonicIndex: Int = 6
  override protected implicit val majorKey: MajorKey =
    MajorKey(ascending(1)).get
}

object Locrian {
  def apply(tonic: String): Option[Locrian] =
    ConstructorUtils.validTonicConstructor(tonic, s => new Locrian(s))
}
