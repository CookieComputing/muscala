package scale
import chord.seventh.{
  DominantSeventh,
  HalfDiminishedSeventh,
  MajorSeventh,
  MinorSeventh
}
import chord.triad.{DiminishedTriad, MajorTriad, MinorTriad}
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.MinorScaleTest.naturalMinorScaleGen

class MinorScaleTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a natural minor scale should have the expected interval " +
      "distances") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      scale <- naturalMinorScaleGen
    } yield (scale, octave)) {
      case (scale: NaturalMinorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          Note.enharmonic(notes.head, notes.head.perfect.unison) &&
            Note.enharmonic(notes(1), notes.head.major.second) &&
            Note.enharmonic(notes(2), notes.head.minor.third) &&
            Note.enharmonic(notes(3), notes.head.perfect.fourth) &&
            Note.enharmonic(notes(4), notes.head.perfect.fifth) &&
            Note.enharmonic(notes(5), notes.head.minor.sixth) &&
            Note.enharmonic(notes(6), notes.head.minor.seventh)
        )
    }
  }

  property("a natural minor scale should follow the W-H-W-W-H-W-W pattern") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      scale <- naturalMinorScaleGen
    } yield (scale, octave)) {
      case (scale: NaturalMinorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          notes.head.rank + 2 == notes(1).rank &&
            notes(1).rank + 1 == notes(2).rank &&
            notes(2).rank + 2 == notes(3).rank &&
            notes(3).rank + 2 == notes(4).rank &&
            notes(4).rank + 1 == notes(5).rank &&
            notes(5).rank + 2 == notes(6).rank &&
            notes(6).rank + 2 == notes.head.perfect.octave.rank
        )
    }
  }

  property(
    "a natural minor scale's descending order should be the reverse of " +
      "it's ascending order") {
    forAll(naturalMinorScaleGen) { naturalMinorScale: NaturalMinorScale =>
      assert(
        naturalMinorScale.ascending.reverse == naturalMinorScale.descending)
    }
  }

  property(
    "a natural minor scale's triads should follow the expected chord triads"
  ) {
    forAll(naturalMinorScaleGen) { minorScale: NaturalMinorScale =>
      minorScale.triads match {
        case List(MinorTriad(_),
                  DiminishedTriad(_),
                  MajorTriad(_),
                  MinorTriad(_),
                  MinorTriad(_),
                  MajorTriad(_),
                  MajorTriad(_)) =>
          succeed
        case _ => fail("triad pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each triad in a natural minor scales's triads should " +
      "be scale degrees found in the minor scale") {
    forAll(naturalMinorScaleGen) { minorScale: NaturalMinorScale =>
      assert(minorScale.triads.forall(triad =>
        triad.tones.toSet.subsetOf(minorScale.ascending.toSet)))
    }
  }

  property(
    "a natural minor key's seventh chords should follow the expected seventh " +
      "chords"
  ) {
    forAll(naturalMinorScaleGen) { scale: NaturalMinorScale =>
      scale.sevenths match {
        case List(
            MinorSeventh(_),
            HalfDiminishedSeventh(_),
            MajorSeventh(_),
            MinorSeventh(_),
            MinorSeventh(_),
            MajorSeventh(_),
            DominantSeventh(_)
            ) =>
          succeed
        case _ => fail("seventh chord pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each seventh in a natural minor scale's seventh " +
      "should be scale degrees found in the minor key") {
    forAll(naturalMinorScaleGen) { scale: NaturalMinorScale =>
      assert(scale.sevenths.forall(seventh =>
        seventh.tones.toSet.subsetOf(scale.ascending.toSet)))
    }
  }
}

object MinorScaleTest extends OptionValues {
  val naturalMinorScaleGen: Gen[NaturalMinorScale] = for {
    letter <- NoteTest.noteLetterGen
  } yield NaturalMinorScale(letter.toString).value
}
