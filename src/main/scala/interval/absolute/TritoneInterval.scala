package interval.absolute
import interval.Interval
import note.Note

/**
  * An intermediary class which can apply a tritone interval
  * distance to a given root note.
  */
case class TritoneInterval(root: Note) extends Interval {

  /**
    * Creates a note a tritone away from the root.
    * @return a note a tritone away from the root
    */
  def tritoneInterval: Note = move(6)
}
