package util
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.OptionValues

/**
  * Helpful utilities that could be used for notes.
  */
object NoteUtil extends OptionValues {

  // Provides a cleared string for a note where conflicting accidentals are
  // removed
  val clearedNoteStringGen: Gen[String] = for {
    letter <- NoteTest.noteLetterGen
    // Limited to avoid intensive CPU time
    numOfAccidentals <- Gen.chooseNum(0, 1000)
    accidental <- NoteTest.accidentalGen
  } yield letter.toString + accidental.toString * numOfAccidentals

  // Provides a cleared note where any conflicting accidentals are removed
  val clearedNoteGen: Gen[Note] = for {
    name <- clearedNoteStringGen
    octave <- Gen.chooseNum(-10000, 10000)
  } yield Note(name, octave).value
}
