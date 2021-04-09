package interval.diatonic
import interval.diatonic.DiatonicIntervalTest.{diatonicOps, keyGen}
import key.{Key, MajorKeyTest, MinorKeyTest}
import note.{Note, NoteTest}
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class DiatonicIntervalTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a note that is enharmonic with any note in a key should return a " +
      "new note for any diatonic method") {
    forAll(for {
      key <- keyGen
      note <- NoteTest.noteGen
    } yield (key, note)) {
      case (key: Key, note: Note) =>
        val nearestNotes = key.degrees.map(Note(_).get.nearestNote)

        assert(
          if (nearestNotes.exists(n => n.name == note.nearestNote.name))
            diatonicOps(key).forall(op => op(note).isDefined)
          else diatonicOps(key).forall(op => op(note).isEmpty))
    }
  }

  property(
    "diatonic intervals for major keys should result in major" +
      "key interval distances between notes") {
    forAll(MajorKeyTest.majorKeyGen) { key: Key =>
      {
        implicit val impKey: Key = key
        val note = Note(key.tonic).get

        val diatonicAndMajors = diatonicOps(key) zip
          List(
            note.perfect.unison,
            note.major.second,
            note.major.third,
            note.perfect.fourth,
            note.perfect.fifth,
            note.major.sixth,
            note.major.seventh,
            note.perfect.octave
          )

        assert(diatonicAndMajors.forall(tup =>
          Note.enharmonic(tup._1(note).get, tup._2)))
      }
    }
  }

  property(
    "diatonic intervals for minor keys should result in minor" +
      "key interval distances between notes") {
    forAll(MinorKeyTest.minorKeyGen) { key: Key =>
      {
        implicit val impKey: Key = key
        val note = Note(key.tonic).get

        val diatonicAndMajors = diatonicOps(key) zip
          List(
            note.perfect.unison,
            note.major.second,
            note.minor.third,
            note.perfect.fourth,
            note.perfect.fifth,
            note.minor.sixth,
            note.minor.seventh,
            note.perfect.octave
          )

        assert(diatonicAndMajors.forall(tup =>
          Note.enharmonic(tup._1(note).get, tup._2)))
      }
    }
  }

  property(
    "diatonic intervals should appropriately extract the scale degrees " +
      "relative to the placement of a note"
  ) {
    forAll(for {
      key <- keyGen
      position <- Gen.chooseNum(0, 6)
    } yield (key, position)) {
      case (key: Key, position: Int) =>
        val note = Note(key.degrees(position)).get

        val diatonicAndOffset = diatonicOps(key) zip (0 to 7)
        assert(
          diatonicAndOffset.forall(tup =>
            tup._1(note).get.name ==
              key.degrees((position + tup._2) % key.degrees.size)))
    }
  }
}

// Utilities for testing diatonic intervals
object DiatonicIntervalTest {
  val keyGen: Gen[Key] =
    Gen.oneOf(MajorKeyTest.majorKeyGen, MinorKeyTest.minorKeyGen)

  val diatonicOps: Key => List[Note => Option[Note]] = (key: Key) =>
    List(
      DiatonicInterval.unison(_: Note)(key),
      DiatonicInterval.second(_: Note)(key),
      DiatonicInterval.third(_: Note)(key),
      DiatonicInterval.fourth(_: Note)(key),
      DiatonicInterval.fifth(_: Note)(key),
      DiatonicInterval.sixth(_: Note)(key),
      DiatonicInterval.seventh(_: Note)(key),
      DiatonicInterval.octave(_: Note)(key)
  )
}
