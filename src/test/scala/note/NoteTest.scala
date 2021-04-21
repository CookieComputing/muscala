package note
import note.NoteTest.{
  accidentalGen,
  conventionalNotes,
  nearestNoteTable,
  noteGen
}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.prop.TableFor2
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._

import scala.util.Random

/**
  * Properties for the Note class.
  */
class NoteTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  // Adding larger coverage for Note, since Note is a fundamental concept to
  // the library
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)

  property("a valid note should only have letters from 'A' to 'G'") {
    forAll(for {
      char <- Gen.alphaChar
      numOfAccidentals <- Gen.chooseNum(1, 1000)
      accidentals <- Gen.listOfN(numOfAccidentals, accidentalGen)
    } yield Note(char.toString ++ accidentals.toString)) { note: Option[Note] =>
      note match {
        case Some(Note(name, _)) =>
          assert(('A' to 'G').contains(name.head))
        case None =>
          succeed
      }
    }
  }

  property("a note should be copyable from its name and octave") {
    forAll(NoteTest.noteGen) { note: Note =>
      assert(note == Note(note.name, note.octave).get)
    }
  }

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

  property(
    "nearestNote() should convert a note's name to a conventional " +
      "note") {
    forAll(NoteTest.noteGen) { note: Note =>
      assert(conventionalNotes.contains(note.nearestNote.name))
    }
  }

  property("nearestNote() should pass the provided table tests") {
    forAll(nearestNoteTable) {
      case (actual: String, expected: String) =>
        val note = Note(actual).value
        assertResult(expected)(note.nearestNote.name)
    }
  }

  property("nearestNote() causes the note to have one accidental at most") {
    forAll(NoteTest.accidentalNoteGen) { note: Note =>
      assert(note.nearestNote.accidentals.length <= 1)
    }
  }

  property("one octave is twelve half steps") {
    forAll(NoteTest.noteGen) { note: Note =>
      val aboveOctaveNote = Note(note.name, note.octave + 1).get
      val belowOctaveNote = Note(note.name, note.octave - 1).get

      assert(
        (Note.distance(note, aboveOctaveNote) == 12) &&
          (Note.distance(belowOctaveNote, note) == 12))
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

  property(
    "two notes that are potentially of two different octaves but the " +
      "same note should be similar"
  ) {
    forAll(for {
      note <- noteGen
      otherOctave <- Gen.chooseNum(-10000, 10000)
    } yield (note, otherOctave)) {
      case (note: Note, otherOctave: Int) =>
        val otherNote = Note(note.name, otherOctave).get
        assert(Note.similarNotes(note, otherNote))
    }
  }

  property(
    "if two notes are not similar, then they cannot share a rank " +
      "modulo 12") {
    forAll(for {
      note <- noteGen
      otherNote <- noteGen suchThat { n =>
        !Note.similarNotes(n, note)
      }
    } yield (note, otherNote)) {
      case (note: Note, otherNote: Note) =>
        assert(Math.floorMod(Note.distance(note, otherNote),
                             Note.halfStepsInOctave) != 0)
    }
  }

  property("enharmonic notes are similar") {
    forAll(for {
      note <- noteGen
      numOfAccidentals <- Gen.chooseNum(0, 100)
    } yield (note, numOfAccidentals)) {
      case (note: Note, numOfAccidentals: Int) =>
        val enharmonicNote = Random
          .shuffle(
            (1 to numOfAccidentals).map(_ => Note.sharp)
              ++ (1 to
                numOfAccidentals).map(_ => Note.flat))
          .foldRight(note) {
            case (char: Char, acc: Note) =>
              char match {
                case Note.flat  => acc.flat
                case Note.sharp => acc.sharp
              }
          }

        assert(
          Note.enharmonic(note, enharmonicNote) &&
            Note.similarNotes(note, enharmonicNote))
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

  val conventionalNotes: List[String] = List(
    "C",
    "C#",
    "Db",
    "D",
    "D#",
    "Eb",
    "E",
    "F",
    "F#",
    "Gb",
    "G",
    "G#",
    "Ab",
    "A",
    "A#",
    "Bb",
    "B"
  )

  val nearestNoteTable: TableFor2[String, String] = Table(
    ("accidental", "expected"),
    ("C##", "D"),
    ("C#####", "F"),
    ("Dbbb", "B"),
    ("A####", "C#"),
    ("G#####bb", "A#"),
    ("G#b#b#b#b", "G"),
    ("F##", "G"),
    ("E#", "F"),
    ("Fb", "E"),
    ("B#", "C"),
    ("Cb", "B"),
    ("A#", "A#"),
    ("Gb", "Gb")
  )
}
