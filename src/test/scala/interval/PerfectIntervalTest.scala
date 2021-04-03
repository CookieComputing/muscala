package interval
import note.Note
import note.NoteTest.noteGen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Represents tests for the Perfect interval.
  */
class PerfectIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a perfect unison is the same note") {
    forAll(noteGen) { note: Note =>
      assert(note.perfect.unison == note)
    }
  }

  property("a perfect fourth is a whole step above a major third") {
    forAll(noteGen) { note: Note =>
      assert(note.perfect.fourth.rank == note.major.third.rank + 1)
    }
  }

  property("a perfect fourth is a whole step below a perfect fifth") {
    forAll(noteGen) { note: Note =>
      assert(note.perfect.fourth.rank == note.perfect.fifth.rank - 2)
    }
  }

  property("a perfect fifth is a stacked major third + minor third") {
    forAll(noteGen) { note: Note =>
      assert(
        note.major.third.minor.third.rank == note.perfect.fifth.rank &&
          note.minor.third.major.third.rank == note.perfect.fifth.rank)
    }
  }

  property("a perfect octave is 12 half steps away from the note") {
    forAll(noteGen) { note: Note =>
      assert(
        note.octave == note.perfect.octave.octave - 1 &&
          note.rank == note.perfect.octave.rank - Note.halfStepsInOctave)
    }
  }
}
