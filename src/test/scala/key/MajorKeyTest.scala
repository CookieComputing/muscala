package key
import key.MajorKeyTest.{circleOfFifthsTable, majorKeyGen}
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.prop.TableFor2
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._

class MajorKeyTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a major key's scales can be generated as notes") {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(key.degrees.map(MajorKey(_)).forall(_.isDefined))
    }
  }

  property("a major key's scales should not have conflicting accidentals") {
    forAll(majorKeyGen) { key: MajorKey =>
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

  property("a major key should have 7 unique letters in it's scales") {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(key.degrees.toSet.size == key.degrees.size)
    }
  }

  property("a major key should have the same notes as it's relative minor") {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(key.degrees.toSet == key.relativeMinor.degrees.toSet)
    }
  }

  property(
    "given a table of tonics and their corresponding scales from the circle " +
      "of fifths, " +
      "generating these keys should return the expected scales") {
    forAll(circleOfFifthsTable) {
      case (tonic: String, expectedDegrees: List[String]) =>
        assert(MajorKey(tonic).get.degrees == expectedDegrees)
    }
  }
}

// Utility for generating
object MajorKeyTest {
  val majorKeyGen: Gen[MajorKey] = for {
    letter <- NoteTest.noteLetterGen
    accidental <- NoteTest.accidentalGen
    // limited to save CPU time
    numOfAccidentals <- Gen.choose(0, 100)
  } yield
    MajorKey(letter.toString + (accidental.toString * numOfAccidentals)).get

  val circleOfFifthsTable: TableFor2[String, List[String]] =
    Table(
      ("tonic", "degrees"),
      ("Cb", List("Cb", "Db", "Eb", "Fb", "Gb", "Ab", "Bb")),
      ("Gb", List("Gb", "Ab", "Bb", "Cb", "Db", "Eb", "F")),
      ("Db", List("Db", "Eb", "F", "Gb", "Ab", "Bb", "C")),
      ("Ab", List("Ab", "Bb", "C", "Db", "Eb", "F", "G")),
      ("Eb", List("Eb", "F", "G", "Ab", "Bb", "C", "D")),
      ("Bb", List("Bb", "C", "D", "Eb", "F", "G", "A")),
      ("F", List("F", "G", "A", "Bb", "C", "D", "E")),
      ("C", List("C", "D", "E", "F", "G", "A", "B")),
      ("G", List("G", "A", "B", "C", "D", "E", "F#")),
      ("D", List("D", "E", "F#", "G", "A", "B", "C#")),
      ("A", List("A", "B", "C#", "D", "E", "F#", "G#")),
      ("E", List("E", "F#", "G#", "A", "B", "C#", "D#")),
      ("B", List("B", "C#", "D#", "E", "F#", "G#", "A#")),
      ("F#", List("F#", "G#", "A#", "B", "C#", "D#", "E#")),
      ("C#", List("C#", "D#", "E#", "F#", "G#", "A#", "B#"))
    )
}
