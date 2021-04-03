package note
import note.NoteTest.noteGen
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Properties for the Note class.
  */
class NoteTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a sharp note is one half step above the previous note") {
    forAll(NoteTest.noteGen) { note: Note =>
      {
        val sharpNote = note.sharp
        assert(Note.distance(note, sharpNote) == 1)
      }
    }
  }

  property("a flat note is one half step below the previous note") {
    forAll(NoteTest.noteGen) { note: Note =>
      {
        val flatNote = note.flat
        assert(Note.distance(note, flatNote) == -1)
      }
    }
  }

  property("a sharp and flat note cancel each other out") {
    forAll(NoteTest.noteGen) { note: Note =>
      assert(
        note.sharp.flat.rank == note.flat.sharp.rank &&
          note.flat.sharp.rank == note.rank)
    }
  }

  property(
    "clearConflictingCharacters() has an accidental count <= the" +
      "original note's accidental count") {
    forAll(NoteTest.noteGen) { note: Note =>
      {
        val originalAccidentalCount = note.name.drop(1).length
        assert(
          note.clearConflictingAccidentals.accidentals.length <=
            originalAccidentalCount)
      }
    }
  }

  property(
    "clearConflictingCharacters" +
      "() should only have " +
      "sharps or only flats or neither") {
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

  property("clearConflictingCharacters() should preserve a note's rank") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      assert(note.clearConflictingAccidentals.rank == note.rank)
    }
  }

  property(
    "clearConflictingCharacters() should not eliminate accidentals" +
      "that do not conflict with each other") {
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

  property(
    "clearConflictingAccidentals() should remove notes that were " +
      "sharped and then flatted") {
    forAll(NoteTest.naturalNoteGen) { note: Note =>
      val alteredNote = note.sharp.flat
      val clearedNote = alteredNote.clearConflictingAccidentals
      assert(note.name == clearedNote.name)
    }
  }

  property("nearestNote() preserves a note's rank") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      assert(note.nearestNote.rank == note.rank)
    }
  }

  // TODO: Maybe add a property that verifies that nearest note will change
  //  a note, idk

  property("nearestNote() causes the note to have one accidental at most") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      assert(note.nearestNote.accidentals.length <= 1)
    }
  }

  property("one octave is twelve half steps") {
    forAll(NoteTest.noteGen) { note: Note =>
      // There are minor edge cases like B# which will up the octave
      // unexpectedly, which is why we use nearest note
      val clearNote = note.nearestNote
      val aboveOctaveNote = Note(clearNote.name, note.octave + 1).get
      val belowOctaveNote = Note(clearNote.name, note.octave - 1).get

      assert(
        (Note.distance(clearNote, aboveOctaveNote) == 12) &&
          (Note.distance(belowOctaveNote, clearNote) == 12))
    }
  }

  property("a sharpened note will always return a positive distance") {
    forAll(
      for {
        // Had to limit the number of times a note was sharpened because of
        // CPU time
        x <- Gen.chooseNum(1, 10000)
        note <- noteGen
      } yield (note, x)
    ) {
      case (note: Note, x: Int) =>
        val sharpNote = (1 to x).foldLeft(note) { (n, _) =>
          n.sharp
        }
        assert(Note.distance(note, sharpNote) > 0)
    }
  }

  property("a flattened note will always return a negative distance") {
    forAll(
      for {
        // Had to limit the number of times a note was sharpened because of
        // CPU time
        x <- Gen.chooseNum(1, 10000)
        note <- noteGen
      } yield (note, x)
    ) {
      case (note: Note, x: Int) =>
        val flatNote = (1 to x).foldLeft(note) { (n, _) =>
          n.flat
        }
        assert(Note.distance(note, flatNote) < 0)
    }
  }

  property("a note should be enharmonic with itself") {
    forAll(noteGen) { note: Note =>
      assert(Note.enharmonic(note, note))
    }
  }

  property(
    "a note that has been altered in any way should not enharmonic " +
      "with itself") {
    forAll(for {
      x <- Gen.chooseNum(-10000, 10000) suchThat { _ != 0 }
      note <- noteGen
    } yield (note, x)) {
      case (note: Note, x: Int) =>
        val op = (n: Note) => if (x > 0) n.sharp else n.flat
        val alteredNote = (1 to math.abs(x)).foldLeft(note) { (n, _) =>
          op(n)
        }
        assertResult(false)(Note.enharmonic(note, alteredNote))
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
