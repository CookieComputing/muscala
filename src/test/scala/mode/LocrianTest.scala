package mode
import chord.triad.DiminishedTriad
import interval.absolute.Interval.{halfStep, wholeStep}
import mode.LocrianTest.locrianModeGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.NaturalMinorScale
import util.NoteUtil

class LocrianTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("the locrian mode should follow the pattern H-W-W-H-W-W-W") {
    forAll(for {
      mode <- locrianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Locrian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(
          Note.distance(notes.head, notes(1)) == halfStep &&
            Note.distance(notes(1), notes(2)) == wholeStep &&
            Note.distance(notes(2), notes(3)) == wholeStep &&
            Note.distance(notes(3), notes(4)) == halfStep &&
            Note.distance(notes(4), notes(5)) == wholeStep &&
            Note.distance(notes(5), notes(6)) == wholeStep &&
            Note.distance(notes(6), notes.head.perfect.octave) == wholeStep
        )
    }
  }

  property(
    "the locrian mode can be thought of as a minor scale with a " +
      "lowered second and fifth") {
    forAll(locrianModeGen) { mode: Locrian =>
      val minorScaleNotes = NaturalMinorScale(mode.tonic).value.ascending

      assertResult(
        minorScaleNotes
          .updated(1, Note.flatName(minorScaleNotes(1)))
          .updated(4, Note.flatName(minorScaleNotes(4))))(mode.ascending)
    }
  }

  property("the tonic chord should be a diminished triad") {
    forAll(locrianModeGen) { mode: Locrian =>
      assertResult(DiminishedTriad(mode.ascending.head).value.tones)(
        List(mode.ascending.head, mode.ascending(2), mode.ascending(4)))
    }
  }

  property("the locrian mode should have a diminished fifth tritone") {
    forAll(for {
      mode <- locrianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Locrian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(Note.enharmonic(notes(4), notes.head.tritone))
    }
  }
}

object LocrianTest extends OptionValues {
  val locrianModeGen: Gen[Locrian] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield Locrian(tonic).value
}
