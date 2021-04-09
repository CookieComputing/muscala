package chord

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
}
