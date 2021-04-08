package interval.absolute
import interval.Interval
import note.Note

/**
  * An intermediary class which can apply an augmented interval of some
  * distance to a given root note.
  */
case class AugmentedInterval(root: Note) extends Interval {

  /**
    * Creates a note an augmented unison away from the root.
    * @return a note an augmented unison away from the root
    */
  def unison: Note = move(1)

  /**
    * Creates a note an augmented second away from the root.
    * @return a note an augmented second away from the root
    */
  def second: Note = move(3)

  /**
    * Creates a note an augmented third away from the root.
    * @return a note an augmented third away from the root
    */
  def third: Note = move(5)

  /**
    * Creates a note an augmented fourth away from the root.
    * @return a note an augmented fourth away from the root
    */
  def fourth: Note = move(6)

  /**
    * Creates a note an augmented fifth away from the root.
    * @return a note an augmented fifth away from the root
    */
  def fifth: Note = move(8)

  /**
    * Creates a note an augmented sixth away from the root.
    * @return a note an augmented sixth away from the root
    */
  def sixth: Note = move(10)

  /**
    * Creates a note an augmented seventh away from the root.
    * @return a note an augmented seventh away from the root
    */
  def seventh: Note = move(12)
}
