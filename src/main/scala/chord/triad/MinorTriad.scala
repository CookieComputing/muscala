package chord.triad
import key.MinorKey
import note.Note

case class MinorTriad private (tonic: String) extends Triad {
  override val key: MinorKey = MinorKey(tonic).get
}

object MinorTriad {
  def apply(tonic: String): Option[MinorTriad] =
    if (Note.tonicRegex.matches(tonic)) Some(new MinorTriad(tonic)) else None
}
