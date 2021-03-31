package note

import org.scalacheck._

/**
  * Properties for the Note class.
  */
object NoteTest extends Properties("Note") {
  import Prop.{forAll, propBoolean}

  val noteLetterGen: Gen[Char] =
    Gen.oneOf(List('A', 'B', 'C', 'D', 'E', 'F', 'G'))

  val naturalNoteGen: Gen[Note] = for {
    letter <- noteLetterGen
  } yield Note(letter.toString).get

  val noteGen: Gen[Note] = for {
    letter <- noteLetterGen
    numOfAccidentals <- Gen.chooseNum(0, 2)
    accidentals <- Gen.listOfN(
      numOfAccidentals,
      Gen.oneOf(Gen.const(Note.flat), Gen.const(Note.sharp)))
    octave <- Gen.chooseNum(0, 5)
  } yield Note(letter.toString + accidentals.mkString(""), octave).get

  val accidentalNoteGen: Gen[Note] = for {
    note <- noteGen
    accidental <- Gen.oneOf(List(Note.flat, Note.sharp))
  } yield Note(note.name + accidental, note.octave).get

  // A sharp note is one half step above its natural
  property("sharpNoteOneHalfStepAboveNatural") = forAll(naturalNoteGen) {
    naturalNote: Note =>
      {
        val sharpNote = naturalNote.sharp
        sharpNote.rank == naturalNote.rank + 1
      }
  }

  // A flat note is one half step below its natural
  property("flatNoteOneHalfStepBelowNatural") = forAll(naturalNoteGen) {
    naturalNote: Note =>
      {
        val flatNote = naturalNote.flat
        flatNote.rank == naturalNote.rank - 1
      }
  }

  property("sharpNoteOneHalfStepAboveNatural") = forAll(noteGen) { note: Note =>
    {
      val sharpNote = note.sharp
      sharpNote.rank == note.rank + 1
    }
  }

  property("flatNoteOneHalfStepBelowNatural") = forAll(noteGen) { note: Note =>
    {
      val flatNote = note.flat
      flatNote.rank == note.rank - 1
    }
  }

  property("sharpAndFlatNeutralizeEachOther") = forAll(noteGen) { note: Note =>
    note.sharp.flat.rank == note.flat.sharp.rank &&
    note.flat.sharp.rank == note.rank
  }

  property("clearConflictingCharactersHasLessThanOrEqualToOriginalCount") =
    forAll(noteGen) { note: Note =>
      {
        val originalAccidentalCount = note.name.drop(1).length
        note.clearConflictingAccidentals.name
          .drop(1)
          .length <= originalAccidentalCount
      }
    }

  property("clearConflictingCharactersOnlyHasSharpsOrFlatsOrNone") =
    forAll(accidentalNoteGen) { note: Note =>
      {
        val clearedNote = note.clearConflictingAccidentals
        val sharpCount = clearedNote.name.drop(1).count(_ == Note.sharp)
        val flatCount = clearedNote.name.drop(1).count(_ == Note.flat)
        (sharpCount == 0 && flatCount > 0) ||
        (sharpCount > 0 && flatCount == 0) ||
        (sharpCount == 0 && flatCount == 0)
      }
    }

  property("clearConflictingCharactersPreservesRank") =
    forAll(accidentalNoteGen) { note: Note =>
      note.clearConflictingAccidentals.rank == note.rank
    }

  property(
    "clearConflictingCharactersShouldNotEliminateNonconflictingAccidentals") =
    forAll(accidentalNoteGen) { note: Note =>
      {
        def accidentalCount(n: Note, char: Char) =
          n.name.drop(1).count(_ == char)
        val originalSharpCount = accidentalCount(note, Note.sharp)
        val originalFlatCount = accidentalCount(note, Note.flat)

        val clearedNote = note.clearConflictingAccidentals
        val sharpCount = clearedNote.name.drop(1).count(_ == Note.sharp)
        val flatCount = clearedNote.name.drop(1).count(_ == Note.flat)

        if (originalFlatCount > originalSharpCount)
          flatCount == originalFlatCount - originalSharpCount && sharpCount == 0
        else if (originalSharpCount > originalFlatCount)
          sharpCount == originalSharpCount - originalFlatCount && flatCount == 0
        else sharpCount == 0 && flatCount == 0
      }
    }

  property("clearConflictingAccidentalsShouldRemoveSharpFlatNote") =
    forAll(naturalNoteGen) { note: Note =>
      val alteredNote = note.sharp.flat
      val clearedNote = alteredNote.clearConflictingAccidentals
      note.name == clearedNote.name
    }

  property("nearestNotePreservesRank") = forAll(accidentalNoteGen) {
    note: Note =>
      note.nearestNote.rank == note.rank
  }

  property("nearestNoteHasOneAccidentalAtMost") = forAll(accidentalNoteGen) {
    note: Note =>
      (note.nearestNote.name.drop(1).length <= 1) :| s"Note did not have " +
        s"at most one accidental: ${note.nearestNote.name}"
  }

  property("oneOctaveAboveNaturalIsTwelveHalfSteps") = forAll(noteGen) {
    note: Note =>
      // There are minor edge cases like B# which will up the octave
      // unexpectedly, which is why we use nearest note
      val clearNote = note.nearestNote
      val octaveNote = Note(clearNote.name, note.octave + 1).get
      (octaveNote.rank == clearNote.rank + 12) :| s"note, rank: $clearNote, " +
        s"${clearNote.rank} | octave note, rank: $octaveNote, ${octaveNote.rank}"
  }
}
