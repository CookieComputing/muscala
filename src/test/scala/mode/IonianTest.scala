package mode
import interval.absolute.Interval.{halfStep, wholeStep}
import mode.IonianTest.ionianModeGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.MajorScale
import util.NoteUtil

class IonianTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("the ionian mode should have the same scale as the major scale") {
    forAll(for {
      tonic <- NoteUtil.clearedNoteStringGen
      majorScale <- MajorScale(tonic)
      ionianMode <- Ionian(tonic)
    } yield (majorScale, ionianMode)) {
      case Some((majorScale: MajorScale, ionian: Ionian)) =>
        assertResult(majorScale.ascending)(ionian.ascending)
      case _ =>
        fail("Expected successful construction of major scale and ionian mode")
    }
  }

  property("the ionian mode should follow the W-W-H-W-W-W-H interval") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      mode <- ionianModeGen
    } yield (mode, octave)) {
      case (mode: Mode, octave: Int) =>
        val notes = mode.toNotes(octave)
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
}

object IonianTest extends OptionValues {
  val ionianModeGen: Gen[Ionian] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield Ionian(tonic).value
}
