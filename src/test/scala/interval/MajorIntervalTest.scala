package interval
import note.Note
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import note.NoteTest.noteGen

/**
  * Represents tests for the Major interval.
  */
class MajorIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a major second is a whole step above the root") {
    forAll(noteGen) { note: Note =>
      assert(note.major.second.rank == note.rank + 2)
    }
  }

  property("a major third is a whole step above a major second") {
    forAll(noteGen) { note: Note =>
      assert(note.major.third.rank == note.major.second.rank + 2)
    }
  }

  property("a major third is a half step below a perfect fourth") {
    forAll(noteGen) { note: Note =>
      assert(note.major.third.rank == note.perfect.fourth.rank - 1)
    }
  }

  property("a major sixth is a whole step above a perfect fifth") {
    forAll(noteGen) { note: Note =>
      assert(note.major.sixth.rank == note.perfect.fifth.rank + 2)
    }
  }

  property("a major sixth is a whole step below a major seventh") {
    forAll(noteGen) { note: Note =>
      assert(note.major.sixth.rank == note.major.seventh.rank - 2)
    }
  }

  property("a major seventh is a half step below the octave") {
    forAll(noteGen) { note: Note =>
      assert(note.major.seventh.rank == note.perfect.octave.rank - 1)
    }
  }
}
