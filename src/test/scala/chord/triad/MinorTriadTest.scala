package chord.triad
import chord.triad.MinorTriadTest.minorTriadGen
import key.{MinorKey, MinorKeyTest}
import note.Note
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class MinorTriadTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("a minor triad should have three tones") {
    forAll(minorTriadGen) { triad: MinorTriad =>
      assert(triad.tones.size == 3)
    }
  }

  property(
    "a minor triad should be a minor third, and a major third stacked " +
      "on top") {
    forAll(minorTriadGen) { triad: MinorTriad =>
      val (root, third, fifth) = triad.toNotes() match {
        case List(r, t, f) => (r, t, f)
      }

      assert(
        Note.enharmonic(root, root.perfect.unison) &&
          Note.enharmonic(third, root.minor.third) &&
          Note.enharmonic(fifth, third.major.third)
      )
    }
  }

  property(
    "a minor triad should be taking the first, third, and fifth scale degrees" +
      " of the key it's associated with"
  ) {
    forAll(minorTriadGen) { triad: MinorTriad =>
      val key = MinorKey(triad.tonic).get
      assert(
        triad.root == key.degrees.head &&
          triad.third == key.degrees(2) &&
          triad.fifth == key.degrees(4)
      )
    }
  }
}

// utilities for testing minor triads
object MinorTriadTest {
  val minorTriadGen: Gen[MinorTriad] = for {
    key <- MinorKeyTest.minorKeyGen
  } yield MinorTriad(key.tonic).get
}
