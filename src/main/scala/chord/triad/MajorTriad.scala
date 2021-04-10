package chord.triad
import key.MajorKey

case class MajorTriad private (tonic: String) extends Triad {
  override val key: MajorKey = MajorKey(tonic).get
}

object MajorTriad {
  def apply(tonic: String): Option[MajorTriad] =
    if (MajorKey(tonic).isDefined) Some(new MajorTriad(tonic)) else None
}
