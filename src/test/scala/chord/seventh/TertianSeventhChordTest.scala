package chord.seventh
import chord.seventh.TertianSeventhChordTest.seventhChordGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Property tests for tertian seventh chords
  */
class TertianSeventhChordTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a seventh chord should have 4 tones") {
    forAll(seventhChordGen) { seventh: Seventh =>
      assertResult(4)(seventh.tones.size)
    }
  }

  property(
    "a seventh chord's toNote() method should have the exact same " +
      "note letters as the tones provided") {
    forAll(for {
      chord <- seventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (chord, octave)) {
      case (seventh: Seventh, octave: Int) =>
        val notes = seventh.toNotes(octave)
        assertResult(seventh.tones)(notes.map(_.name))
    }
  }

  property(
    "a tertiary seventh chord should be a stack of thirds on top of " +
      "each other, regardless of the quality") {
    forAll(for {
      chord <- seventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (chord, octave)) {
      case (seventh: Seventh, octave: Int) =>
        val notes = seventh.toNotes(octave)
        assert(
          (Note.distance(notes.head, notes(1)) == 3
            || Note.distance(notes.head, notes(1)) == 4) &&
            (Note.distance(notes(1), notes(2)) == 3 ||
              Note.distance(notes(1), notes(2)) == 4) &&
            (Note.distance(notes(2), notes(3)) == 3 ||
              Note.distance(notes(2), notes(3)) == 4)
        )
    }
  }
}

object TertianSeventhChordTest {
  val seventhChordGen: Gen[Seventh] = Gen.oneOf(
    AugmentedMajorSeventhTest.augmentedMajorSeventhChordGen,
    DiminishedSeventhTest.diminishedSeventhChordGen,
    DominantSeventhTest.dominantSeventhChordGen,
    HalfDiminishedSeventhTest.halfDiminishedSeventhChordGen,
    MajorSeventhTest.majorSeventhChordGen,
    MinorMajorSeventhTest.minorMajorSeventhChordGen,
    MinorMajorSeventhTest.minorMajorSeventhChordGen
  )
}
