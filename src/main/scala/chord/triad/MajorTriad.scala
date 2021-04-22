package chord.triad
import key.MajorKey
import util.ConstructorUtils

case class MajorTriad private (tonic: String) extends Triad {
  override val key: MajorKey = MajorKey(tonic).get
}

object MajorTriad {
  def apply(tonic: String): Option[MajorTriad] =
    ConstructorUtils.validTonicConstructor(tonic, s => new MajorTriad(s))
}
