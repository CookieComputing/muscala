package interval
import note.Note

/**
  * Represents an intermediary object which can generate a new note of some
  * interval away from the root note. This is useful for users who want some
  * convenient way of generating new notes a specific distance away from a given
  * note.
  *
  * Note that any processing applied towards the note's name will not be done in
  * this trait. This means that an interval of 2 half steps above "C"
  * will be "C##". Instead, it is up to users to determine when there is an
  * appropriate time to convert the note into a more suitable format.
  */
trait Interval {

  // The root note on which all interval operations will be applied
  val root: Note

  implicit protected val op: Note => Note = (note: Note) => note.sharp

  /**
    * Applies a movement of n half steps away from the root note.
    *
    * @param n n half steps to move towards
    * @param op an operation that will apply a half step movement from a note.
    * @return A note exactly n half steps away from the root note
    */
  protected def move(n: Int)(implicit op: Note => Note): Note = {
    (1 to n).foldLeft(root)((acc, _) => op(acc))
  }
}
