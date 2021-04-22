package chord.triad
import chord.triad.AugmentedTriadTest.augmentedTriadGen
import key.MajorKey
import note.Note
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

class AugmentedTriadTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property("an augmented triad should have three tones") {
    forAll(augmentedTriadGen) { triad: AugmentedTriad =>
      assert(triad.tones.size == 3)
    }
  }

  property(
    "when converting to actual notes, the notes return should match " +
      "the tones of the triad") {
    forAll(augmentedTriadGen) { triad: AugmentedTriad =>
      assertResult(triad.tones)(triad.toNotes().map(_.name))
    }
  }

  property(
    "an augmented triad should be a major third, and a major third stacked " +
      "on top") {
    forAll(for {
      triad <- augmentedTriadGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (triad, octave)) {
      case (triad: AugmentedTriad, octave: Int) =>
        val notes = triad.toNotes(octave)
        val root = notes.head
        val third = notes(1)
        val fifth = notes(2)

        assert(
          Note.enharmonic(root, root.perfect.unison) &&
            Note.enharmonic(third, root.major.third) &&
            Note.enharmonic(fifth, third.major.third)
        )
    }
  }

  property(
    "the fifth of an augmented triad should be an augmented fifth"
  ) {
    forAll(augmentedTriadGen) { triad: AugmentedTriad =>
      val notes = triad.toNotes()
      val root = notes.head
      val fifth = notes(2)

      assert(Note.enharmonic(fifth, root.augmented.fifth))
    }
  }

  property(
    "an augmented triad should be taking the first, third, and fifth scale " +
      "degrees of the key it's associated with"
  ) {
    forAll(augmentedTriadGen) { triad: AugmentedTriad =>
      val key = MajorKey(triad.tonic).get
      assert(
        triad.root == key.degrees.head &&
          triad.third == key.degrees(2) &&
          triad.fifth == Note.sharpName(key.degrees(4))
      )
    }
  }
}

// utilities for testing augmented triads
object AugmentedTriadTest {
  val augmentedTriadGen: Gen[AugmentedTriad] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield AugmentedTriad(tonic).get
}
