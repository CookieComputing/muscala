package chord.seventh
import chord.seventh.HalfDiminishedSeventhTest.halfDiminishedSeventhChordGen
import chord.triad.DiminishedTriad
import key.MinorKey
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

class HalfDiminishedSeventhTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property(
    "a half diminished seventh should have the appropriate notes from a " +
      "diminished triad and minor seventh") {
    forAll(halfDiminishedSeventhChordGen) { seventh: HalfDiminishedSeventh =>
      {
        val diminishedTriad = DiminishedTriad(seventh.tonic).value
        val key = MinorKey(seventh.tonic).value

        assert(
          seventh.root == diminishedTriad.root &&
            seventh.third == diminishedTriad.third &&
            seventh.fifth == diminishedTriad.fifth &&
            seventh.seventh == key.degrees(6) &&
            seventh.tones == List(diminishedTriad.root,
                                  diminishedTriad.third,
                                  diminishedTriad.fifth,
                                  key.degrees(6)))
      }
    }
  }

  property(
    "a half diminished seventh chord should have the intervals of a " +
      "diminished triad along with a minor seventh interval") {
    forAll(for {
      seventh <- halfDiminishedSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: HalfDiminishedSeventh, octave: Int) =>
        val triad = DiminishedTriad(seventh.tonic)
        val triadNotes = triad.value.toNotes(octave)
        assert(
          ((triadNotes ++ List(triadNotes.head.minor.seventh)) zip
            seventh.toNotes(octave)).forall((Note.enharmonic _).tupled))
    }
  }

  property(
    "when converting to actual notes, the notes returned should match " +
      "the tones of the seventh chord") {
    forAll(for {
      seventh <- halfDiminishedSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: HalfDiminishedSeventh, octave: Int) =>
        assertResult(seventh.tones)(seventh.toNotes(octave).map(_.name))
    }
  }
}

object HalfDiminishedSeventhTest extends OptionValues {
  val halfDiminishedSeventhChordGen: Gen[HalfDiminishedSeventh] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield HalfDiminishedSeventh(tonic).value
}
