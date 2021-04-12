package chord.seventh
import chord.seventh.MinorSeventhTest.minorSeventhChordGen
import chord.triad.MinorTriad
import key.{MinorKey, MinorKeyTest}
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MinorSeventhTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("a minor seventh should have 4 chord tones") {
    forAll(minorSeventhChordGen) { seventh: MinorSeventh =>
      assertResult(4)(seventh.tones.size)
    }
  }

  property(
    "a minor seventh should use the root, third, fifth, and seventh " +
      "from a minor key of it's tonic") {
    forAll(for {
      seventh <- minorSeventhChordGen
      key <- MinorKey(seventh.tonic)
    } yield (seventh, key)) { gen =>
      gen.value match {
        case (seventh: MinorSeventh, key: MinorKey) =>
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
    "a minor seventh chord should have the intervals of a minor triad," +
      " along with a minor seventh interval") {
    forAll(minorSeventhChordGen) { seventh: MinorSeventh =>
      val triad = MinorTriad(seventh.tonic)
      val triadNotes = triad.value.toNotes()
      ((triadNotes ++ List(triadNotes.head.minor.seventh)) zip
        seventh.toNotes()).forall((Note.enharmonic _).tupled)
    }
  }

  property(
    "when converting to actual notes, the notes return should match " +
      "the tones of the seventh chord") {
    forAll(minorSeventhChordGen) { seventh: MinorSeventh =>
      assertResult(seventh.tones)(seventh.toNotes().map(_.name))
    }
  }
}

object MinorSeventhTest extends OptionValues {
  val minorSeventhChordGen: Gen[MinorSeventh] = for {
    key <- MinorKeyTest.minorKeyGen
  } yield MinorSeventh(key.tonic).value
}
