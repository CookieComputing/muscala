package interval
import note.Note

/**
  * An intermediary class which can apply a minor interval of some distance to
  * a given root note.
  */
case class MinorInterval(root: Note) extends Interval {
  implicit private val op: Note => Note = (note: Note) => note.sharp

  /**
    * Creates a note a minor second away from the root.
    * @return a note a minor second away from the root
    */
  def second: Note = move(1)

  /**
    * Creates a note a minor third away from the root.
    * @return a note a minor third away from the root
    */
  def third: Note = move(3)

  /**
    * Creates a note a minor sixth away from the root.
    * @return a note a minor sixth away from the root
    */
  def sixth: Note = move(8)
}
