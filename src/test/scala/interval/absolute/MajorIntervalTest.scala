package interval.absolute

import note.Note
import note.NoteTest.noteGen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Represents tests for the Major interval.
  */
class MajorIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a major second is a whole step above the root") {
    forAll(noteGen) { note: Note =>
      assert(Note.distance(note, note.major.second) == 2)
    }
  }

  property("a major third is a whole step above a major second") {
    forAll(noteGen) { note: Note =>
      assert(Note.distance(note.major.second, note.major.third) == 2)
    }
  }

  property("a major third is a half step below a perfect fourth") {
    forAll(noteGen) { note: Note =>
      assert(Note.distance(note.major.third, note.perfect.fourth) == 1)
    }
  }

  property("a major sixth is a whole step above a perfect fifth") {
    forAll(noteGen) { note: Note =>
      assert(Note.distance(note.perfect.fifth, note.major.sixth) == 2)
    }
  }

  property("a major sixth is a whole step below a major seventh") {
    forAll(noteGen) { note: Note =>
      assert(Note.distance(note.major.sixth, note.major.seventh) == 2)
    }
  }

  property("a major seventh is a half step below the octave") {
    forAll(noteGen) { note: Note =>
      assert(Note.distance(note.major.seventh, note.perfect.octave) == 1)
    }
  }
}
