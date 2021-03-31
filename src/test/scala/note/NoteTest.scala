package note

import org.scalacheck._

/**
  * Properties for the Note class.
  */
object NoteTest extends Properties("Note") {
  import Prop.forAll

  val noteLetterGen: Gen[Char] =
    Gen.oneOf(List('A', 'B', 'C', 'D', 'E', 'F', 'G'))

  val accidentalGen: Gen[Char] = Gen.oneOf(List(Note.flat, Note.sharp))
  val naturalNoteGen: Gen[Note] = for {
    letter <- noteLetterGen
  } yield Note(letter.toString).get

  // A sharp note is one half step above its natural
  property("sharpNoteOneHalfStepAboveNatural") = forAll(naturalNoteGen) {
    naturalNote: Note =>
      {
        val sharpNote = Note(naturalNote.name + Note.sharp).get
        sharpNote.rank == naturalNote.rank + 1
      }
  }

  // A flat note is one half step below its natural
  property("flatNoteOneHalfStepBelowNatural") = forAll(naturalNoteGen) {
    naturalNote: Note =>
      {
        val flatNote = Note(naturalNote.name + Note.sharp).get
        flatNote.rank == naturalNote.rank + 1
      }
  }
}
