package key
import key.Direction.{Clockwise, CounterClockwise, Direction}
import note.Note

/**
  * A helper object that provides utilities for constructing keys.
  */
object KeyNavigator {
  // used to determine which direction of the circle of fifths to head towards
  private val keyOrdering = Map(
    'F' -> 1,
    'C' -> 2,
    'G' -> 3,
    'D' -> 4,
    'A' -> 5,
    'E' -> 6,
    'B' -> 7
  )

  /**
    * Returns the direction that the current key should shift towards in the
    * circle of fifths.
    * @param scales the list of scales that are in the current key
    * @param tonic the desired tonic that the note at index should be
    * @param index the index of the note that should be checked
    * @return the direction that indicates which direction the shift should
    *         be towards
    */
  def directionTowards(scales: List[String],
                       tonic: String,
                       index: Int): Direction = {
    def accidentalSum(tonic: String) = tonic.drop(1).foldLeft(0) {
      (acc: Int, c: Char) =>
        c match {
          case Note.sharp => acc + 1
          case Note.flat  => acc - 1
        }
    }
    val currentTonicAccidentals = accidentalSum(scales(index))
    val desiredTonicAccidentals = accidentalSum(tonic)

    if (currentTonicAccidentals < desiredTonicAccidentals) Clockwise
    else if (currentTonicAccidentals > desiredTonicAccidentals) CounterClockwise
    else if (keyOrdering(scales(index).head) < keyOrdering(tonic.head))
      Clockwise
    else if (keyOrdering(scales(index).head) > keyOrdering(tonic.head))
      CounterClockwise
    else Direction.None
  }
}

// A enum representing the directions for the circle of fifths
object Direction extends Enumeration {
  type Direction = Value
  val CounterClockwise, Clockwise, None = Value
}
