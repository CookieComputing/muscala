package scale
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.MajorPentatonicScaleTest.majorPentatonicScaleGen

class MajorPentatonicScaleTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a major pentatonic scale has 5 tones") {
    forAll(majorPentatonicScaleGen) { scale: MajorPentatonicScale =>
      assertResult(5)(scale.ascending.size)
    }
  }

  property("the major pentatonic scale should be a subset of the major scale") {
    forAll(majorPentatonicScaleGen) { scale: MajorPentatonicScale =>
      assert(
        scale.ascending.toSet.subsetOf(
          MajorScale(scale.tonic).get.ascending.toSet
        )
      )
    }
  }

  property(
    "the major pentatonic should be tones 1-2-3-5-6 of the major " +
      "scale") {
    forAll(majorPentatonicScaleGen) { scale: MajorPentatonicScale =>
      val majorScale = MajorScale(scale.tonic).get
      assert(
        scale.ascending.head == majorScale.ascending.head &&
          scale.ascending(1) == majorScale.ascending(1) &&
          scale.ascending(2) == majorScale.ascending(2) &&
          scale.ascending(3) == majorScale.ascending(4) &&
          scale.ascending(4) == majorScale.ascending(5)
      )
    }
  }

  property(
    "the major pentatonic intervals should be the ones expected from the " +
      "major scale"
  ) {
    forAll(for {
      scale <- majorPentatonicScaleGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (scale, octave)) {
      case (scale: MajorPentatonicScale, octave: Int) =>
        val notes = scale.toNotes(octave)
        assert(
          Note.enharmonic(notes.head, notes.head.perfect.unison) &&
            Note.enharmonic(notes(1), notes.head.major.second) &&
            Note.enharmonic(notes(2), notes.head.major.third) &&
            Note.enharmonic(notes(3), notes.head.perfect.fifth) &&
            Note.enharmonic(notes(4), notes.head.major.sixth)
        )
    }
  }
}

object MajorPentatonicScaleTest extends OptionValues {
  val majorPentatonicScaleGen: Gen[MajorPentatonicScale] = for {
    letter <- NoteTest.noteLetterGen
    numOfAccidentals <- Gen.chooseNum(0, 1000)
    accidental <- NoteTest.accidentalGen
  } yield
    MajorPentatonicScale(
      letter.toString + accidental.toString * numOfAccidentals).value
}
