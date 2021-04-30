package rhythm.time

/**
  * A time signature is a convention used in music to indicate the duration
  * of a beat in a measure, as well as how many beats are in a measure.
  */
trait TimeSignature {
  // The number of beats in a measure
  val beats: Int

  // How the duration that takes the beat
  val beatDuration: Int
}
