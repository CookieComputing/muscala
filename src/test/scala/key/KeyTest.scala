package key
import key.KeyTest.anyKeyGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * General properties for keys
  */
class KeyTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  // Adding larger coverage for minor keys, since minor keys is a dependency
  // for chords and scales
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
  property("a key's scales should not have conflicting accidentals") {
    forAll(anyKeyGen) { key: Key =>
      def accidentalCount(note: String) =
        (note.count(_ == Note.flat), note.count(_ == Note.sharp))
      def onlyOneAccidentalType(note: String) = {
        val (flats, sharps) = accidentalCount(note)
        (flats > 0 && sharps == 0) || (flats == 0 && sharps > 0) || (flats ==
          0 && sharps == 0)
      }
      assert(key.degrees.forall(onlyOneAccidentalType))
    }
  }

  property("a key should have 7 unique letters in its scales") {
    forAll(anyKeyGen) { key: Key =>
      assert(
        key.degrees.toSet.size == key.degrees.size &&
          key.degrees.size == 7)
    }
  }

  property(
    "a key should have 7 unique letters in its scales even when" +
      "adjusted via Note.nearestNote()") {
    forAll(anyKeyGen) { key: Key =>
      val adjustedKeyNotes = key.degrees.map(Note(_).get.nearestNote)
      assert(
        adjustedKeyNotes.toSet.size == adjustedKeyNotes.size &&
          adjustedKeyNotes.size == 7)
    }
  }
}

object KeyTest extends OptionValues {
  val anyKeyGen: Gen[Key] =
    Gen.oneOf(MajorKeyTest.majorKeyGen, MinorKeyTest.minorKeyGen)
}
