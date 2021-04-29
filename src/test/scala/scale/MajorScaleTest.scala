package scale
import chord.seventh.{
  DominantSeventh,
  HalfDiminishedSeventh,
  MajorSeventh,
  MinorSeventh
}
import chord.triad.{DiminishedTriad, MajorTriad, MinorTriad}
import interval.absolute.Interval.{halfStep, wholeStep}
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.MajorScaleTest.majorScaleGen
import util.NoteUtil

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
          Note.distance(notes.head, notes(1)) == wholeStep &&
            Note.distance(notes(1), notes(2)) == wholeStep &&
            Note.distance(notes(2), notes(3)) == halfStep &&
            Note.distance(notes(3), notes(4)) == wholeStep &&
            Note.distance(notes(4), notes(5)) == wholeStep &&
            Note.distance(notes(5), notes(6)) == wholeStep &&
            Note.distance(notes(6), notes.head.perfect.octave) == halfStep
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

  property(
    "a major scale's seventh chords should follow the expected seventh chords"
  ) {
    forAll(majorScaleGen) { scale: MajorScale =>
      scale.sevenths match {
        case List(
            MajorSeventh(_),
            MinorSeventh(_),
            MinorSeventh(_),
            MajorSeventh(_),
            DominantSeventh(_),
            MinorSeventh(_),
            HalfDiminishedSeventh(_)
            ) =>
          succeed
        case _ => fail("seventh chord pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each seventh in a major scale's seventh should be " +
      "scale degrees found in the major scale") {
    forAll(majorScaleGen) { scale: MajorScale =>
      assert(scale.sevenths.forall(seventh =>
        seventh.tones.toSet.subsetOf(scale.ascending.toSet)))
    }
  }
}

object MajorScaleTest extends OptionValues {
  val majorScaleGen: Gen[MajorScale] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield MajorScale(tonic).value
}
