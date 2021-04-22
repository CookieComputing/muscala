package key
import interval.diatonic.DiatonicInterval
import key.MinorKeyTest.{circleOfFifthsTable, minorKeyGen}
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.prop.TableFor2
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

class MinorKeyTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  // Adding larger coverage for minor keys, since minor keys is a dependency
  // for chords and scales
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
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

  property(
    "a minor key should have 7 unique letters in its scales even when" +
      "adjusted via Note.nearestNote()") {
    forAll(minorKeyGen) { key: MinorKey =>
      val adjustedKeyNotes = key.degrees.map(Note(_).get.nearestNote)
      assert(
        adjustedKeyNotes.toSet.size == adjustedKeyNotes.size &&
          adjustedKeyNotes.size == 7)
    }
  }

  property("a minor key should have the same notes as its relative major") {
    forAll(minorKeyGen) { key: MinorKey =>
      assert(key.degrees.toSet == key.relativeMajor.degrees.toSet)
    }
  }

  property("a minor key should follow the W-H-W-W-H-W-W pattern") {
    forAll(minorKeyGen) { key: MinorKey =>
      (key.degrees zip List(2, 1, 2, 2, 1, 2, 2)).forall(tup => {
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
        assert(MinorKey(tonic).get.degrees == expectedDegrees)
    }
  }

  property("a minor key's parallel should be a major key with the same tonic") {
    forAll(minorKeyGen) { key: MinorKey =>
      key.parallelKey match {
        case MajorKey(tonic) => assertResult(key.tonic)(tonic)
        case MinorKey(_)     => fail("Expected major key")
      }
    }
  }

  property(
    "a minor key's dominant key should be the perfect fifth above the " +
      "key's tonic") {
    forAll(minorKeyGen) { key: MinorKey =>
      key.dominantKey match {
        case MinorKey(tonic) => assertResult(key.degrees(4))(tonic)
        case MajorKey(_)     => fail("Expected minor key")
      }
    }
  }

  property(
    "a minor key's subdominant key should be the perfect fourth above the " +
      "key's tonic"
  ) {
    forAll(minorKeyGen) { key: MinorKey =>
      key.subdominantKey match {
        case MinorKey(tonic) => assertResult(key.degrees(3))(tonic)
        case MajorKey(_)     => fail("Expected minor key")
      }
    }
  }

  property(
    "any minor key that exists on the circle of fifths is not a " +
      "theoretical key") {
    forAll(circleOfFifthsTable) {
      case (tonic: String, _) =>
        val key = MinorKey(tonic).value
        assert(!key.isTheoreticalKey)
    }
  }

  property(
    "any minor key outside of the circle of fifths is a theoretical " +
      "key") {
    forAll(for {
      key <- minorKeyGen suchThat { key =>
        !circleOfFifthsTable.exists(tup => tup._1 == key.tonic)
      }
    } yield key) { minorKey: MinorKey =>
      assert(minorKey.isTheoreticalKey)
    }
  }
}

// Utility for generating
object MinorKeyTest {
  val minorKeyGen: Gen[MinorKey] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield MinorKey(tonic).get

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
