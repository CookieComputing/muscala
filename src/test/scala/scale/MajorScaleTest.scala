package scale
import chord.triad.{DiminishedTriad, MajorTriad, MinorTriad}
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.MajorScaleTest.majorScaleGen

class MajorScaleTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a major scale should have the expected interval distances") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      scale <- majorScaleGen
    } yield (scale, octave)) {
      case (scale: MajorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          Note.enharmonic(notes.head, notes.head.perfect.unison) &&
            Note.enharmonic(notes(1), notes.head.major.second) &&
            Note.enharmonic(notes(2), notes.head.major.third) &&
            Note.enharmonic(notes(3), notes.head.perfect.fourth) &&
            Note.enharmonic(notes(4), notes.head.perfect.fifth) &&
            Note.enharmonic(notes(5), notes.head.major.sixth) &&
            Note.enharmonic(notes(6), notes.head.major.seventh)
        )
    }
  }

  property("a major scale should follow the W-W-H-W-W-W-H pattern") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      scale <- majorScaleGen
    } yield (scale, octave)) {
      case (scale: MajorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          notes.head.rank + 2 == notes(1).rank &&
            notes(1).rank + 2 == notes(2).rank &&
            notes(2).rank + 1 == notes(3).rank &&
            notes(3).rank + 2 == notes(4).rank &&
            notes(4).rank + 2 == notes(5).rank &&
            notes(5).rank + 2 == notes(6).rank &&
            notes(6).rank + 1 == notes.head.perfect.octave.rank
        )
    }
  }

  property(
    "a major scale's descending order should be the reverse of " +
      "it's ascending order") {
    forAll(majorScaleGen) { majorScale: MajorScale =>
      assert(majorScale.ascending.reverse == majorScale.descending)
    }
  }

  property(
    "a major scales's triads should follow the expected chord triads"
  ) {
    forAll(majorScaleGen) { majorScale: MajorScale =>
      majorScale.triads match {
        case List(MajorTriad(_),
                  MinorTriad(_),
                  MinorTriad(_),
                  MajorTriad(_),
                  MajorTriad(_),
                  MinorTriad(_),
                  DiminishedTriad(_)) =>
          succeed
        case _ => fail("triad pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each triad in a major scale's triads should be " +
      "scale degrees found in the major scale") {
    forAll(majorScaleGen) { scale: MajorScale =>
      assert(scale.triads.forall(triad =>
        triad.tones.toSet.subsetOf(scale.ascending.toSet)))
    }
  }
}

object MajorScaleTest extends OptionValues {
  val majorScaleGen: Gen[MajorScale] = for {
    letter <- NoteTest.noteLetterGen
  } yield MajorScale(letter.toString).value
}
