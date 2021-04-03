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
      assert(Note.distance(note.major.third, note.perfect.fourth) == 1)
    }
  }

  property("a perfect fourth is a whole step below a perfect fifth") {
    forAll(noteGen) { note: Note =>
      assert(Note.distance(note.perfect.fourth, note.perfect.fifth) == 2)
    }
  }

  property("a perfect fifth is a stacked major third + minor third") {
    forAll(noteGen) { note: Note =>
      assert(
        Note.enharmonic(note.major.third.minor.third, note.perfect.fifth) &&
          Note.enharmonic(note.minor.third.major.third, note.perfect.fifth))
    }
  }

  property("a perfect octave is 12 half steps away from the note") {
    forAll(noteGen) { note: Note =>
      assert(
        note.octave == note.perfect.octave.octave - 1 &&
          Note.distance(note, note.perfect.octave) == Note.halfStepsInOctave)
    }
  }
}
