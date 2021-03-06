package interval.absolute
import interval.Interval
import note.Note

/**
  * An intermediary class which can apply a perfect interval of some distance to
  * a given root note.
  */
case class PerfectInterval(root: Note) extends Interval {

  /**
    * Creates a note a unison away from the root.
    * @return a note a unison away from the root
    */
  def unison: Note = move(0)

  /**
    * Creates a note a perfect fourth away from the root.
    * @return a note a perfect fourth away from the root
    */
  def fourth: Note = move(5)

  /**
    * Creates a note a perfect fifth away from the root.
    * @return a note a perfect fifth away from the root
    */
  def fifth: Note = move(7)

  /**
    * Creates a note a perfect octave away from the root.
    * @return a note a perfect octave away from the root
    */
  def octave: Note = move(Note.halfStepsInOctave)
}
