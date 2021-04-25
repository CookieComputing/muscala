package key
import key.KeySignatureTest.{
  accidentalsInKey,
  anyKeyGen,
  expectedAccidentalCount
}
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Property tests that are inspired by the lesson at
  * https://www.musictheory.net/lessons/25.
  */
class KeySignatureTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property(
    "a key should have the expected number of accidentals as defined " +
      "by the key signature calculation method") {
    forAll(anyKeyGen) { key: Key =>
      assertResult(expectedAccidentalCount(key))(accidentalsInKey(key))
    }
  }

  property(
    "a major key should have an accidental score of 3 greater than " +
      "it's parallel minor counterpart") {
    forAll(anyKeyGen) { key: Key =>
      key match {
        case MajorKey(tonic) =>
          assertResult(accidentalsInKey(MinorKey(tonic).value) + 3)(
            accidentalsInKey(key))
        case MinorKey(tonic) =>
          assertResult(accidentalsInKey(MajorKey(tonic).value) - 3)(
            accidentalsInKey(key))
      }
    }
  }
}

object KeySignatureTest {
  val anyKeyGen: Gen[Key] =
    Gen.oneOf(MajorKeyTest.majorKeyGen, MinorKeyTest.minorKeyGen)

  private def expectedAccidentalCount(key: Key): Int =
    key.tonic.drop(1).foldLeft(tonicSignatures(key.tonic.head)) {
      case (acc, char) =>
        char match {
          case Note.sharp => acc + 7
          case Note.flat  => acc - 7
        }
    } + (key match {
      case MajorKey(_) => 0
      case MinorKey(_) => -3
    })

  private def accidentalCount(note: String): Int =
    note.drop(1).foldLeft(0) {
      case (acc, char) =>
        char match {
          case Note.sharp => acc + 1
          case Note.flat  => acc - 1
        }
    }

  private def accidentalsInKey(key: Key): Int =
    key.degrees.map(accidentalCount).sum

  private val tonicSignatures = Map(
    'C' -> 0,
    'D' -> 2,
    'E' -> 4,
    'F' -> -1,
    'G' -> 1,
    'A' -> 3,
    'B' -> 5
  )
}
