package mode
import chord.triad.{DiminishedTriad, MajorTriad, MinorTriad}
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

  property("the fourth should be an augmented fourth") {
    forAll(for {
      mode <- lydianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Lydian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(Note.enharmonic(notes.head.augmented.fourth, notes(3)))
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

  property("the tonic, dominant, and supertonic are all major triads") {
    forAll(lydianModeGen) { mode: Lydian =>
      val tonic = MajorTriad(mode.tonic).value
      val dominant = MajorTriad(mode.ascending(4)).value
      val supertonic = MajorTriad(mode.ascending(1)).value

      assert(
        tonic.tones == List(mode.ascending.head,
                            mode.ascending(2),
                            mode.ascending(4)) &&
          dominant.tones == List(mode.ascending(4),
                                 mode.ascending(6),
                                 mode.ascending(1)) &&
          supertonic.tones == List(mode.ascending(1),
                                   mode.ascending(3),
                                   mode.ascending(5))
      )
    }
  }

  property("the subdominant should be diminished") {
    forAll(lydianModeGen) { mode: Lydian =>
      assertResult(DiminishedTriad(mode.ascending(3)).value.tones)(
        List(mode.ascending(3), mode.ascending(5), mode.ascending.head))
    }
  }

  property("the leading tone, mediant, and submediant are all minor") {
    forAll(lydianModeGen) { mode: Lydian =>
      val leadingTone = MinorTriad(mode.ascending(6)).value
      val mediant = MinorTriad(mode.ascending(2)).value
      val submediant = MinorTriad(mode.ascending(5)).value
      assert(
        leadingTone.tones == List(mode.ascending(6),
                                  mode.ascending(1),
                                  mode.ascending(3)) &&
          mediant.tones == List(mode.ascending(2),
                                mode.ascending(4),
                                mode.ascending(6)) &&
          submediant.tones == List(mode.ascending(5),
                                   mode.ascending.head,
                                   mode.ascending(2))
      )
    }
  }

  property("the lydian mode's fourth should be a tritone with the tonic") {
    forAll(for {
      mode <- lydianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Lydian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(
          Note.enharmonic(notes.head.tritone, notes(3))
        )
    }
  }
}

object LydianTest extends OptionValues {
  val lydianModeGen: Gen[Lydian] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield Lydian(tonic).value
}
