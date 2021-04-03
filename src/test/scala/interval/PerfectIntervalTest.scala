package interval
import note.Note
import note.NoteTest.noteGen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Represents tests for the Perfect interval.
  */
class PerfectIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("PerfectUnisonIsTheSameNote") {
    forAll(noteGen) { note: Note =>
      assert(note.perfect.unison == note)
    }
  }

  property("PerfectFourthIsAHalfStepAboveMajorThird") {
    forAll(noteGen) { note: Note =>
      assert(note.perfect.fourth.rank == note.major.third.rank + 1)
    }
  }

  property("PerfectFourthIsAWholeStepBelowPerfectFifth") {
    forAll(noteGen) { note: Note =>
      assert(note.perfect.fourth.rank == note.perfect.fifth.rank - 2)
    }
  }

  property("PerfectFifthIsAStackedMajorAndMinorThird") {
    forAll(noteGen) { note: Note =>
      assert(
        note.major.third.minor.third.rank == note.perfect.fifth.rank &&
          note.minor.third.major.third.rank == note.perfect.fifth.rank)
    }
  }

  property("PerfectOctaveIsAnOctaveAboveRoot") {
    forAll(noteGen) { note: Note =>
      assert(
        note.octave == note.perfect.octave.octave - 1 &&
          note.rank == note.perfect.octave.rank - Note.halfStepsInOctave)
    }
  }
}
