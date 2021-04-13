package chord.seventh
import chord.Chord
import interval.diatonic.DiatonicInterval
import key.Key
import note.Note

/**
  * A seventh chord. A seventh chord is a triad followed by some sort of
  * seventh interval.
  */
trait Seventh extends Chord {
  // The key that a chord is based off of.
  protected val key: Key

  override lazy val tones: List[String] = List(
    root,
    third,
    fifth,
    seventh
  )

  // Alias for the tonic
  val root: String = tonic

  // The third in the chord
  val third: String

  // The fifth in the chord
  val fifth: String

  // The seventh in the chord
  val seventh: String

  /**
    * Converts the seventh chord to a container of notes
    * @param octave The octave that the root note should be based off of
    *  @return the seventh chord as a list of notes
    */
  override def toNotes(octave: Int): List[Note] = {
    val root = Note(tonic, octave).get
    List(
      DiatonicInterval.unison(root)(key),
      DiatonicInterval.third(root)(key),
      DiatonicInterval.fifth(root)(key),
      DiatonicInterval.seventh(root)(key)
    ).map(_.get)
  }
}
