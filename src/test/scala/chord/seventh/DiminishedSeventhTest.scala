package chord.seventh
import chord.seventh.DiminishedSeventhTest.diminishedSeventhChordGen
import chord.triad.DiminishedTriad
import key.MinorKey
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

class DiminishedSeventhTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property(
    "a diminished seventh should have the appropriate notes from a " +
      "diminished triad and diminished seventh") {
    forAll(diminishedSeventhChordGen) { seventh: DiminishedSeventh =>
      {
        val diminishedTriad = DiminishedTriad(seventh.tonic).value
        val key = MinorKey(seventh.tonic).value

        assert(
          seventh.root == diminishedTriad.root &&
            seventh.third == diminishedTriad.third &&
            seventh.fifth == diminishedTriad.fifth &&
            seventh.seventh == Note.flatName(key.degrees(6)) &&
            seventh.tones == List(diminishedTriad.root,
                                  diminishedTriad.third,
                                  diminishedTriad.fifth,
                                  Note.flatName(key.degrees(6))))
      }
    }
  }

  property(
    "a diminished seventh chord should have the intervals of a diminished " +
      "triad along with a diminished seventh interval") {
    forAll(for {
      seventh <- diminishedSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: DiminishedSeventh, octave: Int) =>
        val triad = DiminishedTriad(seventh.tonic)
        val triadNotes = triad.value.toNotes(octave)
        assert(
          ((triadNotes ++ List(triadNotes.head.diminished.seventh)) zip
            seventh.toNotes(octave)).forall((Note.enharmonic _).tupled))
    }
  }

  property(
    "when converting to actual notes, the notes returned should match " +
      "the tones of the seventh chord") {
    forAll(for {
      seventh <- diminishedSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: DiminishedSeventh, octave: Int) =>
        assertResult(seventh.tones)(seventh.toNotes(octave).map(_.name))
    }
  }
}

object DiminishedSeventhTest extends OptionValues {
  val diminishedSeventhChordGen: Gen[DiminishedSeventh] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield DiminishedSeventh(tonic).value
}
