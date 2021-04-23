package mode
import key.MajorKey
import util.ConstructorUtils

case class Aeolian(tonic: String) extends Mode {
  override protected val tonicIndex = 5

  override protected implicit val majorKey: MajorKey =
    MajorKey(ascending(2)).get
}

object Aeolian {
  def apply(tonic: String): Option[Aeolian] =
    ConstructorUtils.validTonicConstructor(tonic, s => new Aeolian(s))
}
