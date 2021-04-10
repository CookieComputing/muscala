package chord.triad
import chord.triad.MajorTriadTest.majorTriadGen
import key.{MajorKey, MajorKeyTest}
import note.Note
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

// tests for major triads
class MajorTriadTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a major triad should have three tones") {
    forAll(majorTriadGen) { triad: MajorTriad =>
      assert(triad.tones.size == 3)
    }
  }

  property(
    "a major triad should be a major third, and a minor third stacked " +
      "on top") {
    forAll(majorTriadGen) { triad: MajorTriad =>
      val (root, third, fifth) = triad.toNotes() match {
        case List(r, t, f) => (r, t, f)
      }

      assert(
        Note.enharmonic(root, root.perfect.unison) &&
          Note.enharmonic(third, root.major.third) &&
          Note.enharmonic(fifth, third.minor.third)
      )
    }
  }

  property(
    "a major triad should be taking the first, third, and fifth scale degrees" +
      " of the key it's associated with"
  ) {
    forAll(majorTriadGen) { triad: MajorTriad =>
      val key = MajorKey(triad.tonic).get
      assert(
        triad.root == key.degrees.head &&
          triad.third == key.degrees(2) &&
          triad.fifth == key.degrees(4)
      )
    }
  }
}

// utilities for testing major triads
object MajorTriadTest {
  val majorTriadGen: Gen[MajorTriad] = for {
    key <- MajorKeyTest.majorKeyGen
  } yield MajorTriad(key.tonic).get
}
