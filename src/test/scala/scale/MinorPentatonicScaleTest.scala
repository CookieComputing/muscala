package scale
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.MinorPentatonicScaleTest.minorPentatonicScaleGen

class MinorPentatonicScaleTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a minor pentatonic scale has 5 notes") {
    forAll(minorPentatonicScaleGen) { scale: MinorPentatonicScale =>
      assertResult(5)(scale.ascending.size)
    }
  }

  property(
    "the minor pentatonic scale should be a subset of the natural " +
      "minor scale") {
    forAll(minorPentatonicScaleGen) { scale: MinorPentatonicScale =>
      assert(
        scale.ascending.toSet
          .subsetOf(NaturalMinorScale(scale.tonic).get.ascending.toSet))
    }
  }

  property(
    "the minor pentatonic should be tones 1-3-4-5-7 of the natural " +
      "minor scale") {
    forAll(minorPentatonicScaleGen) { pentatonicScale: MinorPentatonicScale =>
      val naturalMinorScale = NaturalMinorScale(pentatonicScale.tonic).get
      assert(
        pentatonicScale.ascending.head == naturalMinorScale.ascending.head &&
          pentatonicScale.ascending(1) == naturalMinorScale.ascending(2) &&
          pentatonicScale.ascending(2) == naturalMinorScale.ascending(3) &&
          pentatonicScale.ascending(3) == naturalMinorScale.ascending(4) &&
          pentatonicScale.ascending(4) == naturalMinorScale.ascending(6))
    }
  }

  property(
    "the minor pentatonic intervals should be the ones expected from the " +
      "natural minor scale") {
    forAll(for {
      scale <- minorPentatonicScaleGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (scale, octave)) {
      case (scale: MinorPentatonicScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          Note.enharmonic(notes.head, notes.head.perfect.unison) &&
            Note.enharmonic(notes(1), notes.head.minor.third) &&
            Note.enharmonic(notes(2), notes.head.perfect.fourth) &&
            Note.enharmonic(notes(3), notes.head.perfect.fifth) &&
            Note.enharmonic(notes(4), notes.head.minor.seventh)
        )
    }
  }
}

object MinorPentatonicScaleTest extends OptionValues {
  val minorPentatonicScaleGen: Gen[MinorPentatonicScale] = for {
    letter <- NoteTest.noteLetterGen
    numOfAccidentals <- Gen.chooseNum(0, 1000)
    accidental <- NoteTest.accidentalGen
  } yield
    MinorPentatonicScale(
      letter.toString + accidental.toString * numOfAccidentals).value
}
