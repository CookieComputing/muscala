package mode
import interval.diatonic.DiatonicInterval
import key.{Direction, Key, KeyNavigator, MajorKey}
import note.Note
import scale.Scale

import scala.annotation.tailrec

/**
  * Represents a modern Western mode. These modes use the same set of notes
  * as the major scale, and are differentiated by the starting interval.
  * There are multiple modes for each note. To apply a mode, you would call
  * the mode onto the tonic of the mode. For instance, "A" Dorian will
  * generate the Dorian with A as the tonic, which is the A minor scale.
  */
trait Mode extends Scale {
  // The index on the major scale that the tonic will be located at.
  protected val tonicIndex: Int

  protected implicit val majorKey: MajorKey

  // Ascending works similarly to the key building process, but requires that
  // the tonic be at a specific index.
  @tailrec
  private def modeConstruction(key: Key = MajorKey("C").get): List[String] = {
    KeyNavigator.directionTowards(key.degrees, tonic, tonicIndex) match {
      case Direction.Clockwise =>
        modeConstruction(key.dominantKey)
      case Direction.CounterClockwise =>
        modeConstruction(key.subdominantKey)
      case Direction.None =>
        key.degrees.drop(tonicIndex) ++ key.degrees.take(tonicIndex)
    }
  }

  private lazy val ascendingCachedResult = modeConstruction()

  override def ascending: List[String] =
    ascendingCachedResult

  override def toNotes(octave: Int): List[Note] = {
    val root = Note(ascending.head, octave).get

    for {
      op <- List(
        DiatonicInterval.unison(_),
        DiatonicInterval.second(_),
        DiatonicInterval.third(_),
        DiatonicInterval.fourth(_),
        DiatonicInterval.fifth(_),
        DiatonicInterval.sixth(_),
        DiatonicInterval.seventh(_)
      )
    } yield op(root).get
  }
}
