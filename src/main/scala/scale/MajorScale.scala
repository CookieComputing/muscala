package scale
import interval.diatonic.DiatonicInterval
import key.MajorKey
import note.Note

/**
  * Represents the major scale, which bases the intervals it uses from the
  * major key.
  */
case class MajorScale private (tonic: String) extends Scale {
  implicit private val key: MajorKey = MajorKey(tonic).get

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

object MajorScale {
  def apply(tonic: String): Option[MajorScale] =
    if (MajorKey(tonic).isDefined) Some(new MajorScale(tonic)) else None
}
