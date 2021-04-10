package chord.triad
import chord.Chord

/**
  * Represents a triad, which is a set of three notes typically stacked in
  * thirds. These chord tones are typically the root, third, and fifth scale
  * degrees of a key.
  */
trait Triad extends Chord {
  // Alias for the tonic
  val root: String = tonic

  // Direct access to the third of the triad
  val third: String

  // Direct access to the fifth of the triad
  val fifth: String
}
