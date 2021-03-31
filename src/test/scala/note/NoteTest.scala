package note

import org.scalacheck._

/**
  * Properties for the Note class.
  */
object NoteTest extends Properties("Note") {
  import Prop.forAll

  val noteLetterGen: Gen[Char] =
    Gen.oneOf(List('A', 'B', 'C', 'D', 'E', 'F', 'G'))

  val naturalNoteGen: Gen[Note] = for {
    letter <- noteLetterGen
  } yield Note(letter.toString).get

  val noteGen: Gen[Note] = for {
    letter <- noteLetterGen
    numOfAccidentals <- Gen.chooseNum(0, 1000)
    accidentals <- Gen.listOfN(
      numOfAccidentals,
      Gen.oneOf(Gen.const(Note.flat), Gen.const(Note.sharp)))
  } yield Note(letter.toString + accidentals.mkString("")).get

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
}
