package key
import key.MinorKeyTest.{circleOfFifthsTable, minorKeyGen}
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MinorKeyTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a minor key's scales can be generated as notes") {
    forAll(minorKeyGen) { key: MinorKey =>
      assert(key.degrees.map(MinorKey(_)).forall(_.isDefined))
    }
  }

  property("a minor key's scales should not have conflicting accidentals") {
    forAll(minorKeyGen) { key: MinorKey =>
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

  property("a minor key should have 7 unique letters in its scales") {
    forAll(minorKeyGen) { key: MinorKey =>
      assert(
        key.degrees.toSet.size == key.degrees.size &&
          key.degrees.size == 7)
    }
  }

  property("a minor key should have the same notes as its relative major") {
    forAll(minorKeyGen) { key: MinorKey =>
      assert(key.degrees.toSet == key.relativeMajor.degrees.toSet)
    }
  }

  property(
    "given a table of tonics and their corresponding scales from the circle " +
      "of fifths, " +
      "generating these keys should return the expected scales") {
    forAll(circleOfFifthsTable) {
      case (tonic: String, expectedDegrees: List[String]) =>
        assert(MinorKey(tonic).get.degrees == expectedDegrees)
    }
  }
}

// Utility for generating
object MinorKeyTest {
  val minorKeyGen: Gen[MinorKey] = for {
    letter <- NoteTest.noteLetterGen
    accidental <- NoteTest.accidentalGen
    // limited to save CPU time
    numOfAccidentals <- Gen.choose(0, 100)
  } yield
    MinorKey(letter.toString + (accidental.toString * numOfAccidentals)).get

  val circleOfFifthsTable: TableFor2[String, List[String]] =
    Table(
      ("tonic", "degrees"),
      ("Ab", List("Ab", "Bb", "Cb", "Db", "Eb", "Fb", "Gb")),
      ("Eb", List("Eb", "F", "Gb", "Ab", "Bb", "Cb", "Db")),
      ("Bb", List("Bb", "C", "Db", "Eb", "F", "Gb", "Ab")),
      ("F", List("F", "G", "Ab", "Bb", "C", "Db", "Eb")),
      ("C", List("C", "D", "Eb", "F", "G", "Ab", "Bb")),
      ("G", List("G", "A", "Bb", "C", "D", "Eb", "F")),
      ("D", List("D", "E", "F", "G", "A", "Bb", "C")),
      ("A", List("A", "B", "C", "D", "E", "F", "G")),
      ("E", List("E", "F#", "G", "A", "B", "C", "D")),
      ("B", List("B", "C#", "D", "E", "F#", "G", "A")),
      ("F#", List("F#", "G#", "A", "B", "C#", "D", "E")),
      ("C#", List("C#", "D#", "E", "F#", "G#", "A", "B")),
      ("G#", List("G#", "A#", "B", "C#", "D#", "E", "F#")),
      ("D#", List("D#", "E#", "F#", "G#", "A#", "B", "C#")),
      ("A#", List("A#", "B#", "C#", "D#", "E#", "F#", "G#"))
    )
}
