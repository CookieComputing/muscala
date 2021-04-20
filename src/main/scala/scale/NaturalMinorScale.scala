package scale
import interval.diatonic.DiatonicInterval
import key.MinorKey
import note.Note

/**
  * Represents the natural minor scale, which bases its scale degrees from
  * the minor key.
  */
case class NaturalMinorScale private (tonic: String) extends Scale {
  implicit private val key: MinorKey = MinorKey(tonic).get

  override def ascending: List[String] = key.degrees

  override def toNotes(octave: Int): List[Note] =
    for {
      op <- List(
        DiatonicInterval.unison(_),
        DiatonicInterval.second(_),
        DiatonicInterval.third(_),
        DiatonicInterval.fourth(_),
        DiatonicInterval.fifth(_),
        DiatonicInterval.sixth(_),
        DiatonicInterval.seventh(_)
      )
      root <- Note(tonic, octave)
    } yield op(root).get
}

object NaturalMinorScale {
  def apply(tonic: String): Option[NaturalMinorScale] =
    if (MinorKey(tonic).isDefined) Some(new NaturalMinorScale(tonic)) else None
}
