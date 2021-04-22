package chord.seventh
import chord.triad.MajorTriad
import interval.diatonic.DiatonicInterval
import key.{Key, MajorKey}
import note.Note

/**
  * Represents a dominant seventh, which is a major triad with a minor 7th.
  */
case class DominantSeventh private (tonic: String) extends Seventh {
  private val triad = MajorTriad(tonic).get

  override protected val key: Key = MajorKey(tonic).get

  override val third: String = triad.third

  override val fifth: String = triad.fifth

  override val seventh: String = Note.flatName(key.degrees(6))

  override def toNotes(octave: Int): List[Note] = {
    val originalNotes = super.toNotes(octave)
    originalNotes.dropRight(1) ++ List(
      DiatonicInterval
        .seventh(originalNotes.head)(key)
        .get
        .flat
        .clearConflictingAccidentals)
  }
}

object DominantSeventh {
  def apply(tonic: String): Option[DominantSeventh] =
    if (Note.tonicRegex.matches(tonic)) Some(new DominantSeventh(tonic))
    else
      None
}
