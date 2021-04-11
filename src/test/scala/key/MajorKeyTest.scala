package key
import chord.triad.{DiminishedTriad, MajorTriad, MinorTriad}
import interval.diatonic.DiatonicInterval
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

  property("a major key should have 7 unique letters in its scales") {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(
        key.degrees.toSet.size == key.degrees.size &&
          key.degrees.size == 7)
    }
  }

  property(
    "a major key should have 7 unique letters in its scales even when" +
      "adjusted via Note.nearestNote()") {
    forAll(majorKeyGen) { key: MajorKey =>
      val adjustedKeyNotes = key.degrees.map(Note(_).get.nearestNote)
      assert(
        adjustedKeyNotes.toSet.size == adjustedKeyNotes.size &&
          adjustedKeyNotes.size == 7)
    }
  }

  property("a major key should have the same notes as its relative minor") {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(key.degrees.toSet == key.relativeMinor.degrees.toSet)
    }
  }

  property("a major key should follow the W-W-H-W-W-W-H pattern") {
    forAll(majorKeyGen) { key: MajorKey =>
      (key.degrees zip List(2, 2, 1, 2, 2, 2, 1)).forall(tup => {
        val note = Note(tup._1).get
        Note.distance(note, DiatonicInterval.second(note)(key).get) == tup._2
      })
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

  property("a major key's parallel should be a minor key with the same tonic") {
    forAll(majorKeyGen) { key: MajorKey =>
      key.parallelKey match {
        case MinorKey(tonic) => assertResult(key.tonic)(tonic)
        case MajorKey(_)     => fail("Expected minor key")
      }
    }
  }

  property(
    "a major key's dominant key should be the perfect fifth above the " +
      "key's tonic") {
    forAll(majorKeyGen) { key: MajorKey =>
      key.dominantKey match {
        case MajorKey(tonic) => assertResult(key.degrees(4))(tonic)
        case MinorKey(_)     => fail("Expected major key")
      }
    }
  }

  property(
    "a major key's subdominant key should be the perfect fourth above the " +
      "key's tonic"
  ) {
    forAll(majorKeyGen) { key: MajorKey =>
      key.subdominantKey match {
        case MajorKey(tonic) => assertResult(key.degrees(3))(tonic)
        case MinorKey(_)     => fail("Expected major key")
      }
    }
  }

  property(
    "a major key's triads should follow the expected chord triads"
  ) {
    forAll(majorKeyGen) { key: MajorKey =>
      key.triads match {
        case List(MajorTriad(_),
                  MinorTriad(_),
                  MinorTriad(_),
                  MajorTriad(_),
                  MajorTriad(_),
                  MinorTriad(_),
                  DiminishedTriad(_)) =>
          succeed
        case _ => fail("triad pattern was not matched")
      }
    }
  }

  property(
    "the chord tones in each triad in a major key's triads should be " +
      "scale degrees found in the major key") {
    forAll(majorKeyGen) { key: MajorKey =>
      key.triads.forall(triad => triad.tones.toSet.subsetOf(key.degrees.toSet))
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
