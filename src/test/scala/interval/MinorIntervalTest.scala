package interval
import note.Note
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import note.NoteTest.noteGen

class MinorIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a minor second is a half step above the root note") {
    forAll(noteGen) { note: Note =>
      {
        assert(Note.distance(note, note.minor.second) == 1)
      }
    }
  }

  property("a minor third is a half step above a major second") {
    forAll(noteGen) { note: Note =>
      {
        assert(Note.distance(note.major.second, note.minor.third) == 1)
      }
    }
  }

  property("a minor third is a half step below a major third") {
    forAll(noteGen) { note: Note =>
      {
        assert(Note.distance(note.minor.third, note.major.third) == 1)
      }
    }
  }

  property("a minor sixth is a half step above a perfect fifth") {
    forAll(noteGen) { note: Note =>
      {
        assert(Note.distance(note.perfect.fifth, note.minor.sixth) == 1)
      }
    }
  }

  property("a minor sixth is a half step below a major sixth") {
    forAll(noteGen) { note: Note =>
      {
        assert(Note.distance(note.minor.sixth, note.major.sixth) == 1)
      }
    }
  }

  property("a minor seventh is a half step above a major sixth") {
    forAll(noteGen) { note: Note =>
      {
        assert(Note.distance(note.major.sixth, note.minor.seventh) == 1)
      }
    }
  }

  property("a minor seventh is a half step below a major seventh") {
    forAll(noteGen) { note: Note =>
      {
        assert(Note.distance(note.minor.seventh, note.major.seventh) == 1)
      }
    }
  }
}
