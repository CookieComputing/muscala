package scale
import note.Note

/**
  * Represents a scale in Western music. A scale is a collection of notes
  * ordered by pitch, either ascending or descending, usually aimed to span a
  * single octave. A scale will typically be defined by its tonic and the
  * interval pattern it follows. Note that the scale defined using this trait
  * is intended to be octave-repeating: While there are edge cases where
  * some scales do not necessarily repeat the same intervals per octave, this
  * trait assumes that they do.
  */
trait Scale {
  // The tonic of the scale, defining the starting tone
  val tonic: String

  /**
    * Returns the tones of the scale in ascending order by pitch
    * @return the tones in the scale by ascending order of pitch
    */
  def ascending: List[String]

  /**
    * Returns the tones of the scale in descending order by pitch
    * @return the tones in the scale by descending order of pitch
    */
  def descending: List[String] = ascending.reverse

  /**
    * Converts the scale to a list of notes, using the octave provided to
    * create the octave for the starting tone, or the tonic.
    * @param octave the octave that the tonic will be created with initially
    * @return a list of notes representing the scale
    */
  def toNotes(octave: Int): List[Note]
}
