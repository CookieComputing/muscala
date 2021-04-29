package scale
import chord.seventh.{
  AugmentedMajorSeventh,
  DiminishedSeventh,
  DominantSeventh,
  HalfDiminishedSeventh,
  MajorSeventh,
  MinorMajorSeventh,
  MinorSeventh
}
import chord.triad.{AugmentedTriad, DiminishedTriad, MajorTriad, MinorTriad}
import interval.absolute.Interval.{halfStep, wholeStep}
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.HarmonicMinorScaleTest.harmonicMinorScaleGen
import util.NoteUtil

class HarmonicMinorScaleTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a harmonic minor scale should be a natural minor scale but with a" +
      " raised seventh") {
    forAll(for {
      letter <- NoteTest.noteLetterGen
      naturalMinor <- NaturalMinorScale(letter.toString)
      harmonicMinor <- HarmonicMinorScale(letter.toString)
    } yield (naturalMinor, harmonicMinor)) {
      case Some(
          (naturalMinor: NaturalMinorScale,
           harmonicMinor: HarmonicMinorScale)) =>
        assertResult(
          naturalMinor.ascending.updated(6,
                                         Note.sharpName(naturalMinor
                                           .ascending(6))))(
          harmonicMinor.ascending)
      case _ => fail("expected successful scale generation")
    }
  }

  property(
    "the harmonic minor's gap between the sixth and seventh degree " +
      "should be 3 half steps") {
    forAll(for {
      scale <- harmonicMinorScaleGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (scale, octave)) {
      case (scale: HarmonicMinorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assertResult(3)(Note.distance(notes(5), notes(6)))
    }
  }

  property(
    "a harmonic minor scale should have the expected interval " +
      "distances") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      scale <- harmonicMinorScaleGen
    } yield (scale, octave)) {
      case (scale: HarmonicMinorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          Note.enharmonic(notes.head, notes.head.perfect.unison) &&
            Note.enharmonic(notes(1), notes.head.major.second) &&
            Note.enharmonic(notes(2), notes.head.minor.third) &&
            Note.enharmonic(notes(3), notes.head.perfect.fourth) &&
            Note.enharmonic(notes(4), notes.head.perfect.fifth) &&
            Note.enharmonic(notes(5), notes.head.minor.sixth) &&
            Note.enharmonic(notes(6), notes.head.major.seventh)
        )
    }
  }

  property(
    "a harmonic minor scale follows the pattern that the natural " +
      "minor scale does, but the seventh is raised by one half step") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      scale <- harmonicMinorScaleGen
    } yield (scale, octave)) {
      case (scale: HarmonicMinorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          Note.distance(notes.head, notes(1)) == wholeStep &&
            Note.distance(notes(1), notes(2)) == halfStep &&
            Note.distance(notes(2), notes(3)) == wholeStep &&
            Note.distance(notes(3), notes(4)) == wholeStep &&
            Note.distance(notes(4), notes(5)) == halfStep &&
            Note.distance(notes(5), notes(6)) == wholeStep + 1 &&
            Note.distance(notes(6), notes.head.perfect.octave) == halfStep
        )
    }
  }

  property(
    "a harmonic minor scale's descending order should be the reverse of " +
      "it's ascending order") {
    forAll(harmonicMinorScaleGen) { scale: HarmonicMinorScale =>
      assert(scale.ascending.reverse == scale.descending)
    }
  }

  property(
    "a harmonic minor scale's triads should follow the expected chord triads"
  ) {
    forAll(harmonicMinorScaleGen) { minorScale: HarmonicMinorScale =>
      minorScale.triads match {
        case List(MinorTriad(_),
                  DiminishedTriad(_),
                  AugmentedTriad(_),
                  MinorTriad(_),
                  MajorTriad(_),
                  MajorTriad(_),
                  DiminishedTriad(_)) =>
          succeed
        case _ => fail("triad pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each triad in a harmonic minor scale's triads should" +
      " be scale degrees found in the minor scale") {
    forAll(harmonicMinorScaleGen) { minorScale: HarmonicMinorScale =>
      assert(minorScale.triads.forall(triad =>
        triad.tones.toSet.subsetOf(minorScale.ascending.toSet)))
    }
  }

  property(
    "a harmonic minor key's seventh chords should follow the expected seventh" +
      " chords"
  ) {
    forAll(harmonicMinorScaleGen) { scale: HarmonicMinorScale =>
      scale.sevenths match {
        case List(
            MinorMajorSeventh(_),
            HalfDiminishedSeventh(_),
            AugmentedMajorSeventh(_),
            MinorSeventh(_),
            DominantSeventh(_),
            MajorSeventh(_),
            DiminishedSeventh(_)
            ) =>
          succeed
        case _ => fail("seventh chord pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each seventh in a harmonic minor scale's seventh " +
      "should be scale degrees found in the harmonic minor scale") {
    forAll(harmonicMinorScaleGen) { scale: HarmonicMinorScale =>
      assert(scale.sevenths.forall(seventh =>
        seventh.tones.toSet.subsetOf(scale.ascending.toSet)))
    }
  }
}

object HarmonicMinorScaleTest extends OptionValues {
  val harmonicMinorScaleGen: Gen[HarmonicMinorScale] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield HarmonicMinorScale(tonic).value
}
