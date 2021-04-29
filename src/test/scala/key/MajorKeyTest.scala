package key
import interval.absolute.Interval.{halfStep, wholeStep}
import interval.diatonic.DiatonicInterval
import key.MajorKeyTest.{circleOfFifthsTable, majorKeyGen}
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.prop.TableFor2
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._
import util.NoteUtil

class MajorKeyTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  // Adding larger coverage for major keys, since major keys is a dependency
  // for chords and scales
  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 100)
  property("a major key's scales can be generated as notes") {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(key.degrees.map(MajorKey(_)).forall(_.isDefined))
    }
  }

  property("a major key should have the same notes as its relative minor") {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(key.degrees.toSet == key.relativeMinor.degrees.toSet)
    }
  }

  property(
    "a major key's subdominant key should have the same notes except for " +
      "one note") {
    forAll(majorKeyGen) { key: MajorKey =>
      assertResult(1)(
        (key.degrees.toSet &~ key.subdominantKey.degrees.toSet).size)
    }
  }

  property(
    "a major key's dominant key should have the same notes except for " +
      "one note") {
    forAll(majorKeyGen) { key: MajorKey =>
      assertResult(1)((key.degrees.toSet &~ key.dominantKey.degrees.toSet).size)
    }
  }

  property("a major key should follow the W-W-H-W-W-W-H pattern") {
    forAll(majorKeyGen) { key: MajorKey =>
      (key.degrees zip List(wholeStep,
                            wholeStep,
                            halfStep,
                            wholeStep,
                            wholeStep,
                            wholeStep,
                            halfStep))
        .forall(tup => {
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
    "any major key that exists on the circle of fifths is not a " +
      "theoretical key") {
    forAll(circleOfFifthsTable) {
      case (tonic: String, _) =>
        val key = MajorKey(tonic).value
        assert(!key.isTheoreticalKey)
    }
  }

  property(
    "any major key outside of the circle of fifths is a theoretical " +
      "key") {
    forAll(for {
      key <- majorKeyGen suchThat { key =>
        !circleOfFifthsTable.exists(tup => tup._1 == key.tonic)
      }
    } yield key) { majorKey: MajorKey =>
      assert(majorKey.isTheoreticalKey)
    }
  }

  property(
    "the key signature of a major key should only return notes with " +
      "accidentals in them"
  ) {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(key.signature.forall(s =>
        s.contains(Note.flat) || s.contains(Note.sharp)))
    }
  }

  property(
    "the key signature is a subset of the degrees of a major key"
  ) {
    forAll(majorKeyGen) { key: MajorKey =>
      assert(key.signature.toSet.subsetOf(key.degrees.toSet))
    }
  }
}

// Utility for generating
object MajorKeyTest {
  val majorKeyGen: Gen[MajorKey] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield MajorKey(tonic).get

  val circleOfFifthsTable: TableFor2[String, List[String]] = {
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
}
