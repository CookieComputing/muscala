package chord.seventh
import chord.seventh.MajorSeventhTest.majorSeventhChordGen
import chord.triad.MajorTriad
import key.MajorKey
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

/**
  * Tests for major seventh chords
  */
class MajorSeventhTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a major seventh should have 4 chord tones") {
    forAll(majorSeventhChordGen) { seventh: MajorSeventh =>
      assertResult(4)(seventh.tones.size)
    }
  }

  property(
    "a major seventh should use the root, third, fifth, and seventh " +
      "from a major key of it's tonic") {
    forAll(for {
      seventh <- majorSeventhChordGen
      key <- MajorKey(seventh.tonic)
    } yield (seventh, key)) { opt =>
      opt.value match {
        case (seventh: MajorSeventh, key: MajorKey) =>
          assert(
            seventh.root == key.degrees.head &&
              seventh.third == key.degrees(2) &&
              seventh.fifth == key.degrees(4) &&
              seventh.seventh == key.degrees(6) &&
              seventh.tones == List(key.degrees.head,
                                    key.degrees(2),
                                    key.degrees(4),
                                    key.degrees(6))
          )
      }
    }
  }

  property(
    "a major seventh chord should have the intervals of a major triad," +
      " along with a major seventh interval") {
    forAll(for {
      seventh <- majorSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: MajorSeventh, octave: Int) =>
        val triad = MajorTriad(seventh.tonic)
        val triadNotes = triad.value.toNotes(octave)
        assert(
          ((triadNotes ++ List(triadNotes.head.major.seventh)) zip
            seventh.toNotes(octave)).forall((Note.enharmonic _).tupled))
    }
  }

  property(
    "when converting to actual notes, the notes return should match " +
      "the tones of the seventh chord") {
    forAll(majorSeventhChordGen) { seventh: MajorSeventh =>
      assertResult(seventh.tones)(seventh.toNotes().map(_.name))
    }
  }
}

object MajorSeventhTest extends OptionValues {
  val majorSeventhChordGen: Gen[MajorSeventh] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield MajorSeventh(tonic).value
}
