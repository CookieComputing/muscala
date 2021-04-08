package interval.absolute

import note.Note
import note.NoteTest.noteGen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Represents tests for the Diminished interval.
  */
class DiminishedIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a diminished interval is a half step below it's minor interval " +
      "counterpart") {
    forAll(noteGen) { (note: Note) =>
      {
        assert(
          Note.distance(note.diminished.second, note.minor.second) == 1 &&
            Note.distance(note.diminished.third, note.minor.third) == 1 &&
            Note.distance(note.diminished.sixth, note.minor.sixth) == 1 &&
            Note.distance(note.diminished.seventh, note.minor.seventh) == 1
        )
      }
    }
  }

  property(
    "a diminished interval is a half step below it's perfect interval " +
      "counterpart") {
    forAll(noteGen) { (note: Note) =>
      assert(
        Note.distance(note.diminished.octave, note.perfect.octave) == 1 &&
          Note.distance(note.diminished.fourth, note.perfect.fourth) == 1 &&
          Note.distance(note.diminished.fifth, note.perfect.fifth) == 1
      )
    }
  }
}
