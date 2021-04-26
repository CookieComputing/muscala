package chord.triad
import chord.triad.DiminishedTriadTest.diminishedTriadGen
import key.MinorKey
import note.Note
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

class DiminishedTriadTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a diminished triad should be a minor third, and a minor third stacked " +
      "on top") {
    forAll(for {
      triad <- diminishedTriadGen
      octave <- Gen.chooseNum(-10000, 10000)
    } yield (triad, octave)) {
      case (triad: DiminishedTriad, octave: Int) =>
        val notes = triad.toNotes(octave)
        val root = notes.head
        val third = notes(1)
        val fifth = notes(2)

        assert(
          Note.enharmonic(root, root.perfect.unison) &&
            Note.enharmonic(third, root.minor.third) &&
            Note.enharmonic(fifth, third.minor.third)
        )
    }
  }

  property(
    "the fifth of a diminished triad should be a diminished fifth"
  ) {
    forAll(diminishedTriadGen) { triad: DiminishedTriad =>
      val notes = triad.toNotes()
      val root = notes.head
      val fifth = notes(2)

      assert(Note.enharmonic(fifth, root.diminished.fifth))
    }
  }

  property(
    "a diminished triad should be taking the first, third, and fifth scale " +
      "degrees of the key it's associated with"
  ) {
    forAll(diminishedTriadGen) { triad: DiminishedTriad =>
      val key = MinorKey(triad.tonic).get
      assert(
        triad.root == key.degrees.head &&
          triad.third == key.degrees(2) &&
          triad.fifth == Note.flatName(key.degrees(4))
      )
    }
  }
}

// utilities for testing diminished triads
object DiminishedTriadTest {
  val diminishedTriadGen: Gen[DiminishedTriad] = for {
    tonic <- NoteUtil.clearedNoteStringGen
  } yield DiminishedTriad(tonic).get
}
