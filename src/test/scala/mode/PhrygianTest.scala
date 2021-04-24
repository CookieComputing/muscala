package mode
import mode.PhrygianTest.phrygianModeGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.NaturalMinorScale
import util.NoteUtil

/**
  * Properties for the Phrygian mode.
  */
class PhrygianTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property(
    "the phrygian mode should be equivalent to the minor scale but " +
      "with a lowered second") {
    forAll(phrygianModeGen) { mode: Phrygian =>
      val minorScaleNotes = NaturalMinorScale(mode.tonic).value
      assertResult(
        minorScaleNotes.ascending.updated(
          1,
          Note.flatName(minorScaleNotes.ascending(1))))(mode.ascending)
    }
  }

  property("the phrygian mode should follow the pattern H-W-W-W-H-W-W") {
    forAll(for {
      mode <- phrygianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Phrygian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(
          Note.distance(notes.head, notes(1)) == 1 &&
            Note.distance(notes(1), notes(2)) == 2 &&
            Note.distance(notes(2), notes(3)) == 2 &&
            Note.distance(notes(3), notes(4)) == 2 &&
            Note.distance(notes(4), notes(5)) == 1 &&
            Note.distance(notes(5), notes(6)) == 2 &&
            Note.distance(notes(6), notes.head.perfect.octave) == 2
        )
    }
  }
}

object PhrygianTest extends OptionValues {
  val phrygianModeGen: Gen[Phrygian] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield Phrygian(tonic).value
}
