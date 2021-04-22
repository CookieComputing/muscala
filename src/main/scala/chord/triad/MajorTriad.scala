package chord.triad
import key.MajorKey
import note.Note

case class MajorTriad private (tonic: String) extends Triad {
  override val key: MajorKey = MajorKey(tonic).get
}

object MajorTriad {
  def apply(tonic: String): Option[MajorTriad] =
    if (Note.tonicRegex.matches(tonic)) Some(new MajorTriad(tonic)) else None
}
