package chord.seventh
import chord.seventh.DiminishedSeventhTest.diminishedSeventhChordGen
import chord.triad.DiminishedTriad
import key.{MinorKey, MinorKeyTest}
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DiminishedSeventhTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a diminished seventh should have 4 chord tones") {
    forAll(diminishedSeventhChordGen) { seventh: DiminishedSeventh =>
      assertResult(4)(seventh.tones.size)
    }
  }

  property(
    "a diminished seventh should have the appropriate notes from a " +
      "diminished triad and diminished seventh") {
    forAll(diminishedSeventhChordGen) { seventh: DiminishedSeventh =>
      {
        val diminishedTriad = DiminishedSeventh(seventh.tonic).value
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
        ((triadNotes ++ List(triadNotes.head.diminished.seventh)) zip
          seventh.toNotes(octave)).forall((Note.enharmonic _).tupled)
    }
  }

  property(
    "when converting to actual notes, the notes returned should match " +
      "the tones of the seventh chord") {
    forAll(for {
      seventh <- Gen.const(DiminishedSeventh("B").get)
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: DiminishedSeventh, octave: Int) =>
        assertResult(seventh.tones)(seventh.toNotes(octave).map(_.name))
    }
  }
}

object DiminishedSeventhTest extends OptionValues {
  val diminishedSeventhChordGen: Gen[DiminishedSeventh] = for {
    key <- MinorKeyTest.minorKeyGen
  } yield DiminishedSeventh(key.tonic).value
}
