package scale
import chord.seventh.{
  AugmentedMajorSeventh,
  DominantSeventh,
  HalfDiminishedSeventh,
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
import scale.MelodicMinorScaleTest.melodicMinorScaleGen
import util.NoteUtil

class MelodicMinorScaleTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a melodic minor scale should be a harmonic minor scale but with a" +
      " raised sixth") {
    forAll(for {
      letter <- NoteTest.noteLetterGen
      harmonicMinor <- HarmonicMinorScale(letter.toString)
      melodicMinor <- MelodicMinorScale(letter.toString)
    } yield (harmonicMinor, melodicMinor)) {
      case Some(
          (harmonicMinorScale: HarmonicMinorScale,
           melodicMinorScale: MelodicMinorScale)) =>
        assertResult(
          harmonicMinorScale.ascending.updated(5,
                                               Note.sharpName(harmonicMinorScale
                                                 .ascending(5))))(
          melodicMinorScale.ascending)
      case _ => fail("expected successful scale generation")
    }
  }

  property(
    "the melodic minor's gap between the sixth and seventh degree " +
      "should be 2 half steps") {
    forAll(for {
      scale <- melodicMinorScaleGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (scale, octave)) {
      case (scale: MelodicMinorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assertResult(2)(Note.distance(notes(5), notes(6)))
    }
  }

  property(
    "a melodic minor scale should have the expected interval " +
      "distances") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      scale <- melodicMinorScaleGen
    } yield (scale, octave)) {
      case (scale: MelodicMinorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          Note.enharmonic(notes.head, notes.head.perfect.unison) &&
            Note.enharmonic(notes(1), notes.head.major.second) &&
            Note.enharmonic(notes(2), notes.head.minor.third) &&
            Note.enharmonic(notes(3), notes.head.perfect.fourth) &&
            Note.enharmonic(notes(4), notes.head.perfect.fifth) &&
            Note.enharmonic(notes(5), notes.head.major.sixth) &&
            Note.enharmonic(notes(6), notes.head.major.seventh)
        )
    }
  }

  property(
    "a melodic minor scale follows the pattern that the harmonic " +
      "minor scale does, but the sixth is raised by one half step") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      scale <- melodicMinorScaleGen
    } yield (scale, octave)) {
      case (scale: MelodicMinorScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          Note.distance(notes.head, notes(1)) == wholeStep &&
            Note.distance(notes(1), notes(2)) == halfStep &&
            Note.distance(notes(2), notes(3)) == wholeStep &&
            Note.distance(notes(3), notes(4)) == wholeStep &&
            Note.distance(notes(4), notes(5)) == halfStep + 1 &&
            Note.distance(notes(5), notes(6)) == wholeStep &&
            Note.distance(notes(6), notes.head.perfect.octave) == halfStep
        )
    }
  }

  property(
    "a melodic minor scale's descending order should be the reverse of " +
      "it's ascending order") {
    forAll(melodicMinorScaleGen) { scale: MelodicMinorScale =>
      assert(scale.ascending.reverse == scale.descending)
    }
  }

  property(
    "a melodic minor scale's triads should follow the expected chord triads"
  ) {
    forAll(melodicMinorScaleGen) { minorScale: MelodicMinorScale =>
      minorScale.triads match {
        case List(MinorTriad(_),
                  MinorTriad(_),
                  AugmentedTriad(_),
                  MajorTriad(_),
                  MajorTriad(_),
                  DiminishedTriad(_),
                  DiminishedTriad(_)) =>
          succeed
        case _ => fail("triad pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each triad in a melodic minor scale's triads should" +
      " be scale degrees found in the minor scale") {
    forAll(melodicMinorScaleGen) { minorScale: MelodicMinorScale =>
      assert(minorScale.triads.forall(triad =>
        triad.tones.toSet.subsetOf(minorScale.ascending.toSet)))
    }
  }

  property(
    "a melodic minor key's seventh chords should follow the expected seventh" +
      " chords"
  ) {
    forAll(melodicMinorScaleGen) { scale: MelodicMinorScale =>
      scale.sevenths match {
        case List(
            MinorMajorSeventh(_),
            MinorSeventh(_),
            AugmentedMajorSeventh(_),
            DominantSeventh(_),
            DominantSeventh(_),
            HalfDiminishedSeventh(_),
            HalfDiminishedSeventh(_)
            ) =>
          succeed
        case _ => fail("seventh chord pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each seventh in a melodic minor scale's seventh " +
      "should be scale degrees found in the harmonic minor scale") {
    forAll(melodicMinorScaleGen) { scale: MelodicMinorScale =>
      assert(scale.sevenths.forall(seventh =>
        seventh.tones.toSet.subsetOf(scale.ascending.toSet)))
    }
  }
}

object MelodicMinorScaleTest extends OptionValues {
  val melodicMinorScaleGen: Gen[MelodicMinorScale] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield MelodicMinorScale(tonic).value
}
