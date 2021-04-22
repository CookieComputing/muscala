package chord.triad
import key.MinorKey
import util.ConstructorUtils

case class MinorTriad private (tonic: String) extends Triad {
  override val key: MinorKey = MinorKey(tonic).get
}

object MinorTriad {
  def apply(tonic: String): Option[MinorTriad] =
    ConstructorUtils.validTonicConstructor(tonic, s => new MinorTriad(s))
}
