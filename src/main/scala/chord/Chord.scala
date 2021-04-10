package chord
import note.Note

/**
  * A chord represents the Western notion of a chord: A collection of pitches
  * often played together. They are a central part of harmony and serve as a
  * vital component of this library.
  */
trait Chord {
  // The tonic of the chord.
  val tonic: String

  // The tones that can be found in a chord. Depending on the chord, the size
  // of this can vary
  val tones: List[String]

  /**
    * Given an octave that the tonic of the chord will be based on, returns a
    * collection of notes that represent the chord. The default is centered
    * around C4-B4.
    * @param octave The octave that the root note should be based off of
    * @return the chord as a list of notes
    */
  def toNotes(octave: Int = 4): List[Note]
}
