package scale
import interval.diatonic.DiatonicInterval
import key.MajorKey
import note.Note
import util.ConstructorUtils

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

  /**
    * Returns the relative minor pentatonic of this scale
    * @return the relative minor pentatonic
    */
  def relativeMinorPentatonic: MinorPentatonicScale =
    // recall that the major pentatonic is tones 1-2-3-5-6
    MinorPentatonicScale(ascending.last).get
}

object MajorPentatonicScale {
  def apply(tonic: String): Option[MajorPentatonicScale] =
    ConstructorUtils.validTonicConstructor(tonic,
                                           s => new MajorPentatonicScale(s))
}
