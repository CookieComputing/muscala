package scale
import chord.seventh.{
  DominantSeventh,
  HalfDiminishedSeventh,
  MinorSeventh,
  Seventh
}
import chord.triad.{DiminishedTriad, MajorTriad, MinorTriad, Triad}
import key.MinorKey
import note.Note
import util.ConstructorUtils

/**
  * Represents a melodic minor scale. The melodic minor scale is a modification
  * of the harmonic minor scale, which raises the sixth degree in order to
  * adjust for the awkward interval gap between the sixth and seventh scale
  * degrees.
  */
case class MelodicMinorScale private (tonic: String) extends Scale {
  private val harmonicMinorScale = HarmonicMinorScale(tonic).get

  override def ascending: List[String] =
    harmonicMinorScale.ascending
      .updated(5, Note.sharpName(harmonicMinorScale.ascending(5)))

  override def toNotes(octave: Int): List[Note] = {
    val notes = harmonicMinorScale.toNotes(octave)
    notes.updated(5, notes(5).sharp.clearConflictingAccidentals)
  }

  /**
    * Returns the triads that can be formed when starting at each scale
    * degree. The index of each triad in the list corresponds to the scale
    * degree used as the tonic of the triad.
    * @return the list of triads for the scale
    */
  def triads: List[Triad] =
    harmonicMinorScale.triads
      .updated(1, MinorTriad(ascending(1)).get)
      .updated(3, MajorTriad(ascending(3)).get)
      .updated(5, DiminishedTriad(ascending(5)).get)

  /**
    * Returns the sevenths that can be formed when starting at each scale
    * degree. The index of each seventh in the list corresponds to the scale
    * degree used as the tonic of the seventh.
    * @return the list of sevenths for the scale
    */
  def sevenths: List[Seventh] =
    harmonicMinorScale.sevenths
      .updated(1, MinorSeventh(ascending(1)).get)
      .updated(3, DominantSeventh(ascending(3)).get)
      .updated(5, HalfDiminishedSeventh(ascending(5)).get)
      .updated(6, HalfDiminishedSeventh(ascending(6)).get)
}

object MelodicMinorScale {
  def apply(tonic: String): Option[MelodicMinorScale] =
    ConstructorUtils.validTonicConstructor(tonic, s => new MelodicMinorScale(s))
}
