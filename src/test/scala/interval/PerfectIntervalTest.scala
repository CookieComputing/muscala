package interval
import note.Note
import note.NoteTest.noteGen
import org.scalacheck.Properties

/**
  * Represents tests for the Perfect interval.
  */
object PerfectIntervalTest extends Properties("PerfectInterval") {
  import org.scalacheck.Prop.forAll

  property("PerfectUnisonIsTheSameNote") = forAll(noteGen) { note: Note =>
    note.perfect.unison == note
  }

  property("PerfectFourthIsAHalfStepAboveMajorThird") = forAll(noteGen) {
    note: Note =>
      note.perfect.fourth.rank == note.major.third.rank + 1
  }

  property("PerfectFourthIsAWholeStepBelowPerfectFifth") = forAll(noteGen) {
    note: Note =>
      note.perfect.fourth.rank == note.perfect.fifth.rank - 2
  }

  property("PerfectFifthIsAStackedMajorAndMinorThird") = forAll(noteGen) {
    note: Note =>
      note.major.third.minor.third.rank == note.perfect.fifth.rank &&
      note.minor.third.major.third.rank == note.perfect.fifth.rank
  }

  property("PerfectOctaveIsAnOctaveAboveRoot") = forAll(noteGen) { note: Note =>
    note.octave == note.perfect.octave.octave - 1 &&
    note.rank == note.perfect.octave.rank - Note.halfStepsInOctave
  }
}
