package mode
import chord.triad.{DiminishedTriad, MajorTriad, MinorTriad}
import mode.MixolydianTest.mixolydianModeGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.MajorScale
import util.NoteUtil

class MixolydianTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("the mixolydian mode has 7 notes") {
    forAll(mixolydianModeGen) { mode: Mixolydian =>
      assertResult(7)(mode.ascending.size)
    }
  }

  property(
    "the mixolydian mode is essentially the major scale with a flat " +
      "seventh") {
    forAll(mixolydianModeGen) { mode: Mixolydian =>
      val majorScale: MajorScale = MajorScale(mode.tonic).value
      assertResult(
        majorScale.ascending.dropRight(1) ++ List(
          Note.flatName(majorScale.ascending.last)
        ))(mode.ascending)
    }
  }

  property("the seventh should be a minor seventh") {
    forAll(for {
      mode <- mixolydianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Mixolydian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(Note.enharmonic(notes.last, notes.head.minor.seventh))
    }
  }

  property("the mixolydian mode should follow the pattern W-W-H-W-W-H-W") {
    forAll(for {
      mode <- mixolydianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Mixolydian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(
          Note.distance(notes.head, notes(1)) == 2 &&
            Note.distance(notes(1), notes(2)) == 2 &&
            Note.distance(notes(2), notes(3)) == 1 &&
            Note.distance(notes(3), notes(4)) == 2 &&
            Note.distance(notes(4), notes(5)) == 2 &&
            Note.distance(notes(5), notes(6)) == 1 &&
            Note.distance(notes(6), notes.head.perfect.octave) == 2
        )
    }
  }

  property("the tonic, subdominant, and subtonic triads should all be major") {
    forAll(mixolydianModeGen) { mode: Mixolydian =>
      val tonicTriad = MajorTriad(mode.tonic).get
      val subdominant = MajorTriad(mode.ascending(3)).get
      val subtonic = MajorTriad(mode.ascending(6)).get

      assert(
        tonicTriad.tones == List(mode.ascending.head,
                                 mode.ascending(2),
                                 mode.ascending(4)) &&
          subdominant.tones == List(mode.ascending(3),
                                    mode.ascending(5),
                                    mode.ascending.head) &&
          subtonic.tones == List(mode.ascending(6),
                                 mode.ascending(1),
                                 mode.ascending(3))
      )
    }
  }

  property("the mediant triad should be diminished") {
    forAll(mixolydianModeGen) { mode: Mixolydian =>
      val mediantTriad = DiminishedTriad(mode.ascending(2)).get

      assertResult(
        List(mode.ascending(2), mode.ascending(4), mode.ascending(6)))(
        mediantTriad.tones)
    }
  }

  property("the supertonic, dominant, and submediant are all minor") {
    forAll(mixolydianModeGen) { mode: Mixolydian =>
      val supertonic = MinorTriad(mode.ascending(1)).get
      val dominant = MinorTriad(mode.ascending(4)).get
      val submediant = MinorTriad(mode.ascending(5)).get

      assert(
        supertonic.tones == List(mode.ascending(1),
                                 mode.ascending(3),
                                 mode.ascending(5)) &&
          dominant.tones == List(mode.ascending(4),
                                 mode.ascending(6),
                                 mode.ascending(1)) &&
          submediant.tones == List(mode.ascending(5),
                                   mode.ascending.head,
                                   mode.ascending(2))
      )
    }
  }
}

object MixolydianTest extends OptionValues {
  val mixolydianModeGen: Gen[Mixolydian] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield Mixolydian(tonic).value
}
