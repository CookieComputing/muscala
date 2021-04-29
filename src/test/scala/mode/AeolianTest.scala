package mode
import interval.absolute.Interval.{halfStep, wholeStep}
import key.MinorKey
import mode.AeolianTest.aeolianModeGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scale.NaturalMinorScale
import util.NoteUtil

/**
  * Property tests for the aeolian mode.
  */
class AeolianTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("an aeolian mode should have the same notes has the minor scale") {
    forAll(aeolianModeGen) { mode: Aeolian =>
      val minorScale = NaturalMinorScale(mode.tonic).get
      assertResult(minorScale.ascending)(mode.ascending)
    }
  }

  property(
    "an aeolian mode should contain the same notes as the relative " +
      "major key associated with the minor scale's key") {
    forAll(aeolianModeGen) { mode: Aeolian =>
      assertResult(for {
        minorKey <- MinorKey(mode.tonic)
      } yield minorKey.relativeMajor.degrees.toSet)(Some(mode.ascending.toSet))
    }
  }

  property("the aeolian mode should follow the W-H-W-W-H-W-W pattern") {
    forAll(for {
      mode <- aeolianModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Aeolian, octave: Int) =>
        val notes = mode.toNotes(octave)
        assert(
          Note.distance(notes.head, notes(1)) == wholeStep &&
            Note.distance(notes(1), notes(2)) == halfStep &&
            Note.distance(notes(2), notes(3)) == wholeStep &&
            Note.distance(notes(3), notes(4)) == wholeStep &&
            Note.distance(notes(4), notes(5)) == halfStep &&
            Note.distance(notes(5), notes(6)) == wholeStep &&
            Note.distance(notes(6), notes.head.perfect.octave) == wholeStep)
    }
  }
}

object AeolianTest extends OptionValues {
  val aeolianModeGen: Gen[Aeolian] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield Aeolian(tonic.toString).value
}
