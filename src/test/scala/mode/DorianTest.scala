package mode
import interval.absolute.Interval.{halfStep, wholeStep}
import mode.DorianTest.dorianModeGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.{MelodicMinorScale, NaturalMinorScale}
import util.NoteUtil

/**
  * Property tests for the dorian mode
  */
class DorianTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property(
    "the dorian mode should be akin to the minor scale with a raised " +
      "(major) sixth") {
    forAll(dorianModeGen) { mode: Dorian =>
      val notes = NaturalMinorScale(mode.tonic).value
      assertResult(
        notes.ascending.updated(5, Note.sharpName(notes.ascending(5))))(
        mode.ascending)
    }
  }

  property("the dorian mode should follow the pattern W-H-W-W-W-H-W") {
    forAll(for {
      mode <- dorianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Dorian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(
          Note.distance(notes.head, notes(1)) == wholeStep &&
            Note.distance(notes(1), notes(2)) == halfStep &&
            Note.distance(notes(2), notes(3)) == wholeStep &&
            Note.distance(notes(3), notes(4)) == wholeStep &&
            Note.distance(notes(4), notes(5)) == wholeStep &&
            Note.distance(notes(5), notes(6)) == halfStep &&
            Note.distance(notes(6), notes.head.perfect.octave) == wholeStep)
    }
  }

  property(
    "the dorian mode should have a minor third and seventh, major " +
      "second and sixth, and perfect fourth and fifth") {
    forAll(for {
      mode <- dorianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Dorian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(
          Note.enharmonic(notes.head.minor.third, notes(2)) &&
            Note.enharmonic(notes.head.minor.seventh, notes(6)) &&
            Note.enharmonic(notes.head.major.second, notes(1)) &&
            Note.enharmonic(notes.head.major.sixth, notes(5)) &&
            Note.enharmonic(notes.head.perfect.fourth, notes(3)) &&
            Note.enharmonic(notes.head.perfect.fifth, notes(4))
        )
    }
  }

  property(
    "the dorian mode can also be thought of as a melodic minor scale with a " +
      "lowered (minor) seventh"
  ) {
    forAll(dorianModeGen) { mode: Dorian =>
      val notes = MelodicMinorScale(mode.tonic).value.ascending
      assertResult(notes.updated(6, Note.flatName(notes(6))))(mode.ascending)
    }
  }
}

object DorianTest extends OptionValues {
  val dorianModeGen: Gen[Dorian] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield Dorian(tonic).value
}
