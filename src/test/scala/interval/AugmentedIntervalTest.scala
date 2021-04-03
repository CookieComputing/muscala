package interval
import note.Note
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import note.NoteTest.noteGen

/**
  * Represents tests for the Augmented interval.
  */
class AugmentedIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "an augmented interval is a half step above it's major interval " +
      "counterpart") {
    forAll(noteGen) { (note: Note) =>
      {
        assert(
          Note.distance(note.major.second, note.augmented.second) == 1 &&
            Note.distance(note.major.third, note.augmented.third) == 1 &&
            Note.distance(note.major.sixth, note.augmented.sixth) == 1 &&
            Note.distance(note.major.seventh, note.augmented.seventh) == 1
        )
      }
    }
  }

  property(
    "an augmented interval is a half step above it's perfect interval " +
      "counterpart") {
    forAll(noteGen) { (note: Note) =>
      assert(
        Note.distance(note, note.augmented.unison) == 1 &&
          Note.distance(note.perfect.fourth, note.augmented.fourth) == 1 &&
          Note.distance(note.perfect.fifth, note.augmented.fifth) == 1
      )
    }
  }
}
