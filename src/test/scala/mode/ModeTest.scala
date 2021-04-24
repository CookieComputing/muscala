package mode
import key.MajorKey
import mode.ModeTest.anyModeGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * General mode property tests
  */
class ModeTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("any mode should have 7 notes") {
    forAll(anyModeGen) { mode: Mode =>
      assertResult(7)(mode.ascending.size)
    }
  }

  property(
    "the locrian and lydian mode should be the only modes which have a" +
      " tritone above its tonic") {
    forAll(for {
      mode <- anyModeGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (mode, octave)) {
      case (mode: Mode, octave: Int) =>
        val notes = mode.toNotes(octave)
        mode match {
          case Locrian(_) =>
            assert(Note.enharmonic(notes.head.tritone, notes(4)))
          case Lydian(_) =>
            assert(Note.enharmonic(notes.head.tritone, notes(3)))
          case _ =>
            assert(!((0 to 6).exists(n =>
              Note.enharmonic(notes.head.tritone, notes(n)))))
        }
    }
  }

  property(
    "each mode should have the same notes of some major scale found " +
      "within its scale degrees") {
    forAll(anyModeGen) { mode: Mode =>
      assert(mode.ascending.exists(tonic =>
        MajorKey(tonic).value.degrees.toSet == mode.ascending.toSet))
    }
  }
}

object ModeTest extends OptionValues {
  val anyModeGen: Gen[Mode] = Gen.oneOf(
    IonianTest.ionianModeGen,
    DorianTest.dorianModeGen,
    PhrygianTest.phrygianModeGen,
    LydianTest.lydianModeGen,
    MixolydianTest.mixolydianModeGen,
    AeolianTest.aeolianModeGen,
    LocrianTest.locrianModeGen
  )
}
