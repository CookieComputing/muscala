package scale
import interval.diatonic.DiatonicInterval
import key.MajorKey
import note.Note

/**
  * Represents the major pentatonic scale, which removes the 4th and 7th tones
  * of the major scale.
  */
case class MajorPentatonicScale(tonic: String) extends Scale {
  private lazy val majorScale = MajorScale(tonic).get
  implicit private val majorKey: MajorKey = MajorKey(tonic).get

  override def ascending: List[String] =
    majorScale.ascending.take(3) ++
      majorScale.ascending.drop(4).dropRight(1)

  override def toNotes(octave: Int): List[Note] = {
    val rootNote = Note(tonic, octave).get

    List(DiatonicInterval.unison(_),
         DiatonicInterval.second(_),
         DiatonicInterval.third(_),
         DiatonicInterval.fifth(_),
         DiatonicInterval.sixth(_)).map(op => op(rootNote).get)
  }
}

object MajorPentatonicScale {
  def apply(tonic: String): Option[MajorPentatonicScale] =
    if (MajorScale(tonic).isDefined) Some(new MajorPentatonicScale(tonic))
    else None
}
