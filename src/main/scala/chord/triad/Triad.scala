package chord.triad
import chord.Chord
import interval.diatonic.DiatonicInterval
import key.Key
import note.Note

/**
  * Represents a triad, which is a set of three notes typically stacked in
  * thirds. These chord tones are typically the root, third, and fifth scale
  * degrees of a key.
  */
trait Triad extends Chord {
  // The key that a chord is based off of.
  protected val key: Key

  // Alias for the tonic
  val root: String = tonic

  override lazy val tones: List[String] = List(
    key.degrees.head,
    key.degrees(2),
    key.degrees(4)
  )

  // Direct access to the third of the triad
  lazy val third: String = tones(1)

  // Direct access to the fifth of the triad
  lazy val fifth: String = tones(2)

  /**
    * Converts the triad to a container of notes
    * @param octave The octave that the root note should be based off of
    *  @return the chord as a list of notes
    */
  override def toNotes(octave: Int): List[Note] = {
    val root = Note(tonic, octave).get
    List(
      DiatonicInterval.unison(root)(key),
      DiatonicInterval.third(root)(key),
      DiatonicInterval.fifth(root)(key)
    ).map(_.get)
  }
}
