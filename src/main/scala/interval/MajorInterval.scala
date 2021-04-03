package interval
import note.Note

/**
  * An intermediary class which can apply a major interval of some distance to
  * a given root note.
  */
case class MajorInterval(root: Note) extends Interval {
  implicit private val op: Note => Note = (note: Note) => note.sharp

  /**
    * Creates a note a major second away from the root.
    * @return a note a major second away from the root
    */
  def second: Note = move(2)

  /**
    * Creates a note a major third away from the root.
    * @return a note a major third away from the root
    */
  def third: Note = move(4)

  /**
    * Creates a note a major sixth away from the root.
    * @return a note a major sixth away from the root
    */
  def sixth: Note = move(9)

  /**
    * Creates a note a major seventh away from the root.
    * @return a note a major seventh away from the root
    */
  def seventh: Note = move(11)
}
