package mode
import mode.ModeTest.anyModeGen
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
