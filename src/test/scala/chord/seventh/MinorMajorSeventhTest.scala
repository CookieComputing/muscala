package chord.seventh
import chord.seventh.MinorMajorSeventhTest.minorMajorSeventhChordGen
import chord.triad.MinorTriad
import key.{MajorKey, MinorKeyTest}
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Tests for the minor major seventh chord
  */
class MinorMajorSeventhTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a minor major seventh should have 4 chord tones") {
    forAll(minorMajorSeventhChordGen) { seventh: MinorMajorSeventh =>
      assertResult(4)(seventh.tones.size)
    }
  }

  property(
    "a minor major seventh should have the appropriate notes from a " +
      "minor triad and major seventh") {
    forAll(minorMajorSeventhChordGen) { seventh: MinorMajorSeventh =>
      {
        val minorTriad = MinorTriad(seventh.tonic).value
        val key = MajorKey(seventh.tonic).value

        assert(
          seventh.root == minorTriad.root &&
            seventh.third == minorTriad.third &&
            seventh.fifth == minorTriad.fifth &&
            seventh.seventh == key.degrees(6) &&
            seventh.tones == List(minorTriad.root,
                                  minorTriad.third,
                                  minorTriad.fifth,
                                  key.degrees(6)))
      }
    }
  }

  property(
    "a minor major seventh chord should have the intervals of a minor " +
      "triad along with a major seventh interval") {
    forAll(for {
      seventh <- minorMajorSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: MinorMajorSeventh, octave: Int) =>
        val triad = MinorTriad(seventh.tonic)
        val triadNotes = triad.value.toNotes(octave)
        assert(
          ((triadNotes ++ List(triadNotes.head.major.seventh)) zip
            seventh.toNotes(octave)).forall((Note.enharmonic _).tupled))
    }
  }

  property(
    "when converting to actual notes, the notes returned should match " +
      "the tones of the seventh chord") {
    forAll(for {
      seventh <- minorMajorSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: MinorMajorSeventh, octave: Int) =>
        assertResult(seventh.tones)(seventh.toNotes(octave).map(_.name))
    }
  }
}

object MinorMajorSeventhTest extends OptionValues {
  val minorMajorSeventhChordGen: Gen[MinorMajorSeventh] = for {
    key <- MinorKeyTest.minorKeyGen
  } yield MinorMajorSeventh(key.tonic).value
}
