package chord.seventh
import chord.seventh.AugmentedMajorSeventhTest.augmentedMajorSeventhChordGen
import chord.triad.AugmentedTriad
import key.MajorKey
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

class AugmentedMajorSeventhTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("an augmented major seventh should have 4 chord tones") {
    forAll(augmentedMajorSeventhChordGen) { seventh: AugmentedMajorSeventh =>
      assertResult(4)(seventh.tones.size)
    }
  }

  property(
    "an augmented major seventh should have the appropriate notes from an" +
      "augmented triad and major seventh") {
    forAll(augmentedMajorSeventhChordGen) { seventh: AugmentedMajorSeventh =>
      {
        val augmentedTriad = AugmentedTriad(seventh.tonic).value
        val key = MajorKey(seventh.tonic).value

        assert(
          seventh.root == augmentedTriad.root &&
            seventh.third == augmentedTriad.third &&
            seventh.fifth == augmentedTriad.fifth &&
            seventh.seventh == key.degrees(6) &&
            seventh.tones == List(augmentedTriad.root,
                                  augmentedTriad.third,
                                  augmentedTriad.fifth,
                                  key.degrees(6)))
      }
    }
  }

  property(
    "an augmented seventh chord should have the intervals of an augmented " +
      "triad along with a major seventh interval") {
    forAll(for {
      seventh <- augmentedMajorSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: AugmentedMajorSeventh, octave: Int) =>
        val triad = AugmentedTriad(seventh.tonic)
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
      seventh <- augmentedMajorSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: AugmentedMajorSeventh, octave: Int) =>
        assertResult(seventh.tones)(seventh.toNotes(octave).map(_.name))
    }
  }
}

object AugmentedMajorSeventhTest extends OptionValues {
  val augmentedMajorSeventhChordGen: Gen[AugmentedMajorSeventh] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield AugmentedMajorSeventh(tonic).value
}
