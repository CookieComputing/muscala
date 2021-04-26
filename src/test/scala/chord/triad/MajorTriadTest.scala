package chord.triad
import chord.triad.MajorTriadTest.majorTriadGen
import key.MajorKey
import note.Note
import org.scalacheck.Gen
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import util.NoteUtil

// tests for major triads
class MajorTriadTest extends AnyPropSpec with ScalaCheckPropertyChecks {
  property(
    "a major triad should be a major third, and a minor third stacked " +
      "on top") {
    forAll(for {
      octave <- Gen.chooseNum(-10000, 10000)
      triad <- majorTriadGen
    } yield (triad, octave)) {
      case (triad: MajorTriad, octave: Int) =>
        val notes = triad.toNotes(octave)
        val root = notes.head
        val third = notes(1)
        val fifth = notes(2)

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
    tonic <- NoteUtil.clearedNoteStringGen
  } yield MajorTriad(tonic).get
}
