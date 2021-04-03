package interval
import note.Note
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import note.NoteTest.noteGen

/**
  * Represents tests for the Tritone interval.
  */
class TritoneIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a tritone is enharmonic with augmented fourths and diminished " +
      "fifths") {
    forAll(noteGen) { note: Note =>
      assert(
        Note.distance(note.tritone, note.augmented.fourth) == 0 &&
          Note.distance(note.tritone, note.diminished.fifth) == 0
      )
    }
  }
}
