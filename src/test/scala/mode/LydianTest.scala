package mode
import mode.LydianTest.lydianModeGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.MajorScale
import util.NoteUtil

class LydianTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a lydian mode should have 7 notes") {
    forAll(lydianModeGen) { mode: Lydian =>
      assertResult(7)(mode.ascending.size)
    }
  }

  property(
    "the lydian mode should effectively be the major scale, but the " +
      "fourth degree is sharp") {
    forAll(lydianModeGen) { mode: Lydian =>
      val majorScale = MajorScale(mode.tonic).value
      assertResult(
        majorScale.ascending
          .updated(3, Note.sharpName(majorScale.ascending(3))))(mode.ascending)
    }
  }

  property("the lydian mode should follow the W-W-W-H-W-W-H pattern") {
    forAll(for {
      mode <- lydianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Lydian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(
          Note.distance(notes.head, notes(1)) == 2 &&
            Note.distance(notes(1), notes(2)) == 2 &&
            Note.distance(notes(2), notes(3)) == 2 &&
            Note.distance(notes(3), notes(4)) == 1 &&
            Note.distance(notes(4), notes(5)) == 2 &&
            Note.distance(notes(5), notes(6)) == 2 &&
            Note.distance(notes(6), notes.head.perfect.octave) == 1
        )
    }
  }
}

object LydianTest extends OptionValues {
  val lydianModeGen: Gen[Lydian] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield Lydian(tonic).value
}
