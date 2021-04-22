package scale
import chord.seventh.{
  AugmentedMajorSeventh,
  DiminishedSeventh,
  DominantSeventh,
  MinorMajorSeventh,
  Seventh
}
import chord.triad.{AugmentedTriad, DiminishedTriad, MajorTriad, Triad}
import key.MinorKey
import note.Note

/**
  * Represents a harmonic minor scale. A harmonic minor scale is a
  * modification of the natural minor scale, but with the seventh scale degree
  * raised by one sharp.
  */
case class HarmonicMinorScale private (tonic: String) extends Scale {
  private val naturalMinorScale = NaturalMinorScale(tonic).get

  override def ascending: List[String] = {
    val degrees = naturalMinorScale.ascending
    degrees.updated(6, Note.sharpName(degrees(6)))
  }

  override def toNotes(octave: Int): List[Note] = {
    val notes = naturalMinorScale.toNotes(octave)
    notes.dropRight(1) ++ List(
      notes.last.sharp.clearConflictingAccidentals
    )
  }

  /**
    * Returns the triads that can be formed when starting at each scale
    * degree. The index of each triad in the list corresponds to the scale
    * degree used as the tonic of the triad.
    * @return the list of triads for the scale
    */
  def triads: List[Triad] =
    naturalMinorScale.triads
      .updated(2, AugmentedTriad(ascending(2)).get)
      .updated(4, MajorTriad(ascending(4)).get)
      .updated(6, DiminishedTriad(ascending(6)).get)

  /**
    * Returns the sevenths that can be formed when starting at each scale
    * degree. The index of each seventh in the list corresponds to the scale
    * degree used as the tonic of the seventh.
    * @return the list of sevenths for the scale
    */
  def sevenths: List[Seventh] =
    naturalMinorScale.sevenths
      .updated(0, MinorMajorSeventh(ascending.head).get)
      .updated(2, AugmentedMajorSeventh(ascending(2)).get)
      .updated(4, DominantSeventh(ascending(4)).get)
      .updated(6, DiminishedSeventh(ascending(6)).get)
}

object HarmonicMinorScale {
  def apply(tonic: String): Option[HarmonicMinorScale] =
    if (Note.tonicRegex.matches(tonic)) Some(new HarmonicMinorScale(tonic))
    else
      None
}
