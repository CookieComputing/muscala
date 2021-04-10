package chord.triad
import key.MinorKey

case class MinorTriad private (tonic: String) extends Triad {
  override val key: MinorKey = MinorKey(tonic).get
}

object MinorTriad {
  def apply(tonic: String): Option[MinorTriad] =
    if (MinorKey(tonic).isDefined) Some(new MinorTriad(tonic)) else None
}
