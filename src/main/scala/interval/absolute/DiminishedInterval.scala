package interval.absolute
import interval.Interval
import note.Note

/**
  * An intermediary class which can apply a diminished interval of some
  * distance to a given root note.
  */
case class DiminishedInterval(root: Note) extends Interval {

  /**
    * Creates a note a diminished second away from the root.
    * @return a note a diminished second away from the root
    */
  def second: Note = move(0)

  /**
    * Creates a note a diminished third away from the root.
    * @return a note a diminished third away from the root
    */
  def third: Note = move(2)

  /**
    * Creates a note a diminished fourth away from the root.
    * @return a note a diminished fourth away from the root
    */
  def fourth: Note = move(4)

  /**
    * Creates a note a diminished fifth away from the root.
    * @return a note a diminished fifth away from the root
    */
  def fifth: Note = move(6)

  /**
    * Creates a note a diminished sixth away from the root.
    * @return a note a diminished sixth away from the root
    */
  def sixth: Note = move(7)

  /**
    * Creates a note a diminished seventh away from the root.
    * @return a note a diminished seventh away from the root
    */
  def seventh: Note = move(9)

  /**
    * Creates a note a diminished octave away from the root.
    * @return a note a diminished octave away from the root
    */
  def octave: Note = move(11)
}
