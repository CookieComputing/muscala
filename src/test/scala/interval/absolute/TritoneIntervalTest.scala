package interval.absolute

import note.Note
import note.NoteTest.noteGen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Represents tests for the Tritone interval.
  */
class TritoneIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a tritone is enharmonic with augmented fourths and diminished " +
      "fifths") {
    forAll(noteGen) { note: Note =>
      assert(
        Note.enharmonic(note.tritone, note.augmented.fourth) &&
          Note.enharmonic(note.tritone, note.diminished.fifth)
      )
    }
  }
}
