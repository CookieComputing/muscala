package chord.triad
import interval.diatonic.DiatonicInterval
import key.MinorKey
import note.Note

case class DiminishedTriad private (tonic: String) extends Triad {
  override protected val key: MinorKey = MinorKey(tonic).get

  override lazy val tones: List[String] = {
    List(
      key.degrees.head,
      key.degrees(2),
      Note.flatName(key.degrees(4))
    )
  }

  override def toNotes(octave: Int = 4): List[Note] = {
    val notes = super.toNotes(octave)
    notes.dropRight(1) ++ List(
      DiatonicInterval
        .fifth(notes.head)(key)
        .get
        .flat
        .copy(name = fifth))
  }
}

object DiminishedTriad {
  def apply(tonic: String): Option[DiminishedTriad] =
    if (MinorKey(tonic).isDefined) Some(new DiminishedTriad(tonic)) else None
}
