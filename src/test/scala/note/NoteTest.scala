package note
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Properties for the Note class.
  */
class NoteTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("sharpNoteOneHalfStepAbovePrevious") {
    forAll(NoteTest.noteGen) { note: Note =>
      {
        val sharpNote = note.sharp
        assert(sharpNote.rank == note.rank + 1)
      }
    }
  }

  property("flatNoteOneHalfStepBelowPrevious") {
    forAll(NoteTest.noteGen) { note: Note =>
      {
        val flatNote = note.flat
        assert(flatNote.rank == note.rank - 1)
      }
    }
  }

  property("sharpAndFlatNeutralizeEachOther") {
    forAll(NoteTest.noteGen) { note: Note =>
      assert(
        note.sharp.flat.rank == note.flat.sharp.rank &&
          note.flat.sharp.rank == note.rank)
    }
  }

  property("clearConflictingCharactersHasLessThanOrEqualToOriginalCount") {
    forAll(NoteTest.noteGen) { note: Note =>
      {
        val originalAccidentalCount = note.name.drop(1).length
        assert(
          note.clearConflictingAccidentals.accidentals.length <=
            originalAccidentalCount)
      }
    }
  }

  property("clearConflictingCharactersOnlyHasSharpsOrFlatsOrNone") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      {
        val clearedNote = note.clearConflictingAccidentals
        val sharpCount = clearedNote.accidentals.count(_ == Note.sharp)
        val flatCount = clearedNote.accidentals.count(_ == Note.flat)
        assert(
          (sharpCount == 0 && flatCount > 0) ||
            (sharpCount > 0 && flatCount == 0) ||
            (sharpCount == 0 && flatCount == 0))
      }
    }
  }

  property("clearConflictingCharactersPreservesRank") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      assert(note.clearConflictingAccidentals.rank == note.rank)
    }
  }

  property(
    "clearConflictingCharactersShouldNotEliminateNonconflictingAccidentals") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      {
        def accidentalCount(n: Note, char: Char) =
          n.accidentals.count(_ == char)
        val originalSharpCount = accidentalCount(note, Note.sharp)
        val originalFlatCount = accidentalCount(note, Note.flat)

        val clearedNote = note.clearConflictingAccidentals
        val sharpCount = clearedNote.accidentals.count(_ == Note.sharp)
        val flatCount = clearedNote.accidentals.count(_ == Note.flat)

        assert(if (originalFlatCount > originalSharpCount)
          flatCount == originalFlatCount - originalSharpCount && sharpCount == 0
        else if (originalSharpCount > originalFlatCount)
          sharpCount == originalSharpCount - originalFlatCount && flatCount == 0
        else sharpCount == 0 && flatCount == 0)
      }
    }
  }

  property("clearConflictingAccidentalsShouldRemoveSharpFlatNote") {
    forAll(NoteTest.naturalNoteGen) { note: Note =>
      val alteredNote = note.sharp.flat
      val clearedNote = alteredNote.clearConflictingAccidentals
      assert(note.name == clearedNote.name)
    }
  }

  property("nearestNotePreservesRank") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      assert(note.nearestNote.rank == note.rank)
    }
  }

  // TODO: Maybe add a property that verifies that nearest note will change
  //  a note, idk

  property("nearestNoteHasOneAccidentalAtMost") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      assert(note.nearestNote.accidentals.length <= 1)
    }
  }

  property("oneOctaveIsTwelveHalfSteps") {
    forAll(NoteTest.noteGen) { note: Note =>
      // There are minor edge cases like B# which will up the octave
      // unexpectedly, which is why we use nearest note
      val clearNote = note.nearestNote
      val aboveOctaveNote = Note(clearNote.name, note.octave + 1).get
      val belowOctaveNote = Note(clearNote.name, note.octave - 1).get

      assert(
        (aboveOctaveNote.rank == clearNote.rank + 12) &&
          (belowOctaveNote.rank == clearNote.rank - 12))
    }
  }
}

// Utilities for testing notes
object NoteTest {
  val noteLetterGen: Gen[Char] =
    Gen.oneOf(List('A', 'B', 'C', 'D', 'E', 'F', 'G'))

  val accidentalGen: Gen[Char] = Gen.oneOf(List(Note.flat, Note.sharp))
  val naturalNoteGen: Gen[Note] = for {
    letter <- noteLetterGen
  } yield Note(letter.toString).get

  val noteGen: Gen[Note] = for {
    letter <- noteLetterGen
    numOfAccidentals <- Gen.chooseNum(0, 1000)
    accidentals <- Gen.listOfN(numOfAccidentals, accidentalGen)
    octave <- Gen.chooseNum(-1000, 1000)
  } yield Note(letter.toString + accidentals.mkString(""), octave).get

  val accidentalNoteGen: Gen[Note] = for {
    note <- noteGen
    accidental <- Gen.oneOf(List(Note.flat, Note.sharp))
  } yield Note(note.name + accidental, note.octave).get
}
