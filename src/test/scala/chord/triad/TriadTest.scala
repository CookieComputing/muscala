package chord.triad
import chord.triad.TriadTest.anyTriadGen
import note.Note
import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

/**
  * General properties that all triads should follow
  */
class TriadTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with OptionValues {
  property("any triad should have 3 notes") {
    forAll(anyTriadGen) { triad: Triad =>
      assertResult(3)(triad.tones.size)
    }
  }

  property("a triad's tones should be its root, third, and fifth") {
    forAll(anyTriadGen) { triad: Triad =>
      assertResult(List(triad.root, triad.third, triad.fifth))(triad.tones)
    }
  }

  property("a triad should be thirds stacked on top of each other") {
    forAll(for {
      triad <- anyTriadGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (triad, octave)) {
      case (triad, octave) =>
        val notes = triad.toNotes(octave)
        assert(
          (Note.distance(notes.head, notes(1)) == 3 ||
            Note.distance(notes.head, notes(1)) == 4) &&
            (Note.distance(notes(1), notes(2)) == 3 ||
              Note.distance(notes(1), notes(2)) == 4)
        )
    }
  }

  property(
    "when converting to actual notes, the notes return should match " +
      "the tones of the triad") {
    forAll(anyTriadGen) { triad: Triad =>
      assertResult(triad.tones)(triad.toNotes().map(_.name))
    }
  }
}

object TriadTest extends OptionValues {
  val anyTriadGen: Gen[Triad] = Gen.oneOf(
    AugmentedTriadTest.augmentedTriadGen,
    DiminishedTriadTest.diminishedTriadGen,
    MajorTriadTest.majorTriadGen,
    MinorTriadTest.minorTriadGen)
}
