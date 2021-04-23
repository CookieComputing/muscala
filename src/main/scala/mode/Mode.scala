package mode
import interval.diatonic.DiatonicInterval
import key.{Key, MajorKey}
import mode.Mode.keyOrdering
import note.Note
import scale.{MajorScale, Scale}

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
    if (key.degrees(tonicIndex) == tonic)
      key.degrees.drop(tonicIndex) ++ key.degrees.take(tonicIndex)
    else if (clockwise(key.degrees(tonicIndex), tonic))
      modeConstruction(key.dominantKey)
    else modeConstruction(key.subdominantKey)
  }

  // Determines if we should go clockwise around the circle of fifths.
  private def clockwise(currentTonic: String, desiredTonic: String): Boolean = {
    def accidentalSum(tonic: String) = tonic.drop(1).foldLeft(0) {
      (acc: Int, c: Char) =>
        c match {
          case Note.sharp => acc + 1
          case Note.flat  => acc - 1
        }
    }
    val currentTonicAccidentals = accidentalSum(currentTonic)
    val desiredTonicAccidentals = accidentalSum(desiredTonic)

    if (currentTonicAccidentals < desiredTonicAccidentals) true
    else if (currentTonicAccidentals > desiredTonicAccidentals) false
    else
      Mode.keyOrdering(currentTonic.head) <
        Mode.keyOrdering(desiredTonic.head)
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

object Mode {
  private val keyOrdering = Map(
    'F' -> 1,
    'C' -> 2,
    'G' -> 3,
    'D' -> 4,
    'A' -> 5,
    'E' -> 6,
    'B' -> 7
  )
}
