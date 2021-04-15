package chord.seventh
import chord.triad.AugmentedTriad
import interval.diatonic.DiatonicInterval
import key.{Key, MajorKey}
import note.Note

/**
  * Represents an augmented major seventh chord.
  */
case class AugmentedMajorSeventh private (tonic: String) extends Seventh {
  private val triad = AugmentedTriad(tonic).get
  override protected val key: Key = MajorKey(tonic).get

  override val third: String = triad.third

  override val fifth: String = triad.fifth

  override val seventh: String = key.degrees(6)

  override def toNotes(octave: Int): List[Note] = {
    val originalNotes = super.toNotes(octave)
    originalNotes.dropRight(2) ++ List(
      DiatonicInterval
        .fifth(originalNotes.head)(key)
        .get
        .sharp
        .clearConflictingAccidentals,
      DiatonicInterval.seventh(originalNotes.head)(key).get
    )
  }
}

object AugmentedMajorSeventh {
  def apply(tonic: String): Option[AugmentedMajorSeventh] =
    if (MajorKey(tonic).isDefined) Some(new AugmentedMajorSeventh(tonic))
    else None
}
