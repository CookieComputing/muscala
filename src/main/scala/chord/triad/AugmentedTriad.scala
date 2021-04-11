package chord.triad
import interval.diatonic.DiatonicInterval
import key.MajorKey
import note.Note

case class AugmentedTriad private (tonic: String) extends Triad {
  override protected val key: MajorKey = MajorKey(tonic).get

  override lazy val tones: List[String] = {
    List(
      key.degrees.head,
      key.degrees(2),
      Note.sharpName(key.degrees(4))
    )
  }

  override def toNotes(octave: Int = 4): List[Note] = {
    val notes = super.toNotes(octave)
    notes.dropRight(1) ++ List(
      DiatonicInterval
        .fifth(notes.head)(key)
        .get
        .sharp
        .copy(name = fifth))
  }
}

object AugmentedTriad {
  def apply(tonic: String): Option[AugmentedTriad] =
    if (MajorKey(tonic).isDefined) Some(new AugmentedTriad(tonic)) else None
}
