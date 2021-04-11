package chord.seventh
import chord.seventh.MajorSeventhTest.majorSeventhChordGen
import chord.triad.MajorTriad
import key.{MajorKey, MajorKeyTest}
import note.Note
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * Tests for major seventh chords
  */
class MajorSeventhTest extends AnyPropSpec with ScalaCheckPropertyChecks {
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
    } yield (seventh, key)) {
      case Some((seventh: MajorSeventh, key: MajorKey)) =>
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

  property(
    "a major seventh chord should have the intervals of a major triad," +
      " along with a major seventh interval") {
    forAll(majorSeventhChordGen) { seventh: MajorSeventh =>
      val triad = MajorTriad(seventh.tonic).get
      val triadNotes = triad.toNotes()
      ((triadNotes ++ List(triadNotes.head.major.seventh)) zip
        seventh.toNotes()).forall((Note.enharmonic _).tupled)
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

object MajorSeventhTest {
  val majorSeventhChordGen: Gen[MajorSeventh] = for {
    key <- MajorKeyTest.majorKeyGen
  } yield MajorSeventh(key.tonic).get
}
