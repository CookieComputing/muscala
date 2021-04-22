package scale
import interval.diatonic.DiatonicInterval
import key.MinorKey
import note.Note

/**
  * Represents a minor pentatonic scale, which removes tones 2 and 6 from the
  * minor scale.
  */
case class MinorPentatonicScale private (tonic: String) extends Scale {
  private lazy val minorScale = NaturalMinorScale(tonic).get
  implicit lazy private val minorKey: MinorKey = MinorKey(tonic).get

  override def ascending: List[String] =
    minorScale.ascending.take(1) ++
      minorScale.ascending.drop(2).dropRight(2) ++
      minorScale.ascending.takeRight(1)

  override def toNotes(octave: Int): List[Note] = {
    val rootNote = Note(tonic, octave).get

    List(DiatonicInterval.unison(_),
         DiatonicInterval.third(_),
         DiatonicInterval.fourth(_),
         DiatonicInterval.fifth(_),
         DiatonicInterval.seventh(_)).map(op => op(rootNote).get)
  }

  /**
    * Returns the relative major pentatonic of this scale
    * @return the relative major pentatonic
    */
  def relativeMajorPentatonic: MajorPentatonicScale = {
    // recall that the minor pentatonic is tones 1-3-4-5-7
    MajorPentatonicScale(ascending(1)).get
  }
}

object MinorPentatonicScale {
  def apply(tonic: String): Option[MinorPentatonicScale] =
    if (Note.tonicRegex.matches(tonic))
      Some(new MinorPentatonicScale(tonic))
    else None
}
