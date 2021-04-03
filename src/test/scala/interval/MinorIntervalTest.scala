package interval
import note.Note
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import note.NoteTest.noteGen

class MinorIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a minor second is a half step above the root note") {
    forAll(noteGen) { note: Note =>
      {
        assert(note.minor.second.rank == note.rank + 1)
      }
    }
  }

  property("a minor third is a half step above a major second") {
    forAll(noteGen) { note: Note =>
      {
        assert(note.minor.third.rank == note.major.second.rank + 1)
      }
    }
  }

  property("a minor third is a half step below a major third") {
    forAll(noteGen) { note: Note =>
      {
        assert(note.minor.third.rank == note.major.third.rank - 1)
      }
    }
  }

  property("a minor sixth is a half step above a perfect fifth") {
    forAll(noteGen) { note: Note =>
      {
        assert(note.minor.sixth.rank == note.perfect.fifth.rank + 1)
      }
    }
  }

  property("a minor sixth is a half step below a major sixth") {
    forAll(noteGen) { note: Note =>
      {
        assert(note.minor.sixth.rank == note.major.sixth.rank - 1)
      }
    }
  }

  property("a minor seventh is a half step above a major sixth") {
    forAll(noteGen) { note: Note =>
      {
        assert(note.minor.seventh.rank == note.major.sixth.rank + 1)
      }
    }
  }

  property("a minor seventh is a half step below a major seventh") {
    forAll(noteGen) { note: Note =>
      {
        assert(note.minor.seventh.rank == note.major.seventh.rank - 1)
      }
    }
  }
}
