package chord.seventh
import chord.triad.MinorTriad
import interval.diatonic.DiatonicInterval
import key.{Key, MajorKey, MinorKey}
import note.Note

/**
  * Represents a minor major seventh chord.
  */
case class MinorMajorSeventh private (tonic: String) extends Seventh {
  private val triad = MinorTriad(tonic).get
  private val major = MajorKey(tonic).get

  override protected val key: Key = MinorKey(tonic).get

  override val third: String = triad.third

  override val fifth: String = triad.fifth

  override val seventh: String = major.degrees(6)

  override def toNotes(octave: Int): List[Note] = {
    val originalNotes = super.toNotes(octave)
    originalNotes.dropRight(1) ++ List(
      DiatonicInterval.seventh(originalNotes.head)(major).get
    )
  }
}

object MinorMajorSeventh {
  def apply(tonic: String): Option[MinorMajorSeventh] =
    if (Note.tonicRegex.matches(tonic)) Some(new MinorMajorSeventh(tonic))
    else
      None
}
