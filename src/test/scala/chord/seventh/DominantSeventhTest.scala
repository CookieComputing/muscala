package chord.seventh
import chord.seventh.DominantSeventhTest.dominantSeventhChordGen
import chord.triad.MajorTriad
import key.MajorKey
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

/**
  * Tests for dominant seventh chords
  */
class DominantSeventhTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a dominant seventh should have 4 chord tones") {
    forAll(dominantSeventhChordGen) { seventh: DominantSeventh =>
      assertResult(4)(seventh.tones.size)
    }
  }

  property(
    "a dominant seventh should have the appropriate notes from a " +
      "major triad and minor seventh") {
    forAll(for {
      seventh <- dominantSeventhChordGen
      key <- MajorKey(seventh.tonic)
    } yield (seventh, key)) { opt =>
      opt.value match {
        case (seventh: DominantSeventh, key: MajorKey) =>
          assert(
            seventh.root == key.degrees.head &&
              seventh.third == key.degrees(2) &&
              seventh.fifth == key.degrees(4) &&
              seventh.seventh == Note.flatName(key.degrees(6)) &&
              seventh.tones == List(key.degrees.head,
                                    key.degrees(2),
                                    key.degrees(4),
                                    Note.flatName(key.degrees(6)))
          )
      }
    }
  }

  property(
    "a dominant seventh chord should have the intervals of a major triad," +
      " along with a minor seventh interval") {
    forAll(for {
      seventh <- dominantSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: DominantSeventh, octave: Int) =>
        val triad = MajorTriad(seventh.tonic)
        val triadNotes = triad.value.toNotes(octave)
        assert(
          ((triadNotes ++ List(triadNotes.head.minor.seventh)) zip
            seventh.toNotes(octave)).forall((Note.enharmonic _).tupled))
    }
  }

  property(
    "when converting to actual notes, the notes return should match " +
      "the tones of the seventh chord") {
    forAll(for {
      seventh <- dominantSeventhChordGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (seventh, octave)) {
      case (seventh: DominantSeventh, octave: Int) =>
        assertResult(seventh.tones)(seventh.toNotes(octave).map(_.name))
    }
  }
}

object DominantSeventhTest extends OptionValues {
  val dominantSeventhChordGen: Gen[DominantSeventh] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield DominantSeventh(tonic).value
}
