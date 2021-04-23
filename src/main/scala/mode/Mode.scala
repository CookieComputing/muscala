package mode
import interval.diatonic.DiatonicInterval
import key.MajorKey
import note.Note
import scale.{MajorScale, Scale}

/**
  * Represents a modern Western mode. These modes use the same set of notes
  * as the major scale, and are differentiated by the starting interval.
  * There are multiple modes for each note.
  */
trait Mode extends Scale {
  // The index on the major scale that the tonic will be located at.
  protected val tonicIndex: Int

  private lazy val majorScale: MajorScale = MajorScale(tonic).get
  private implicit val majorKey: MajorKey = MajorKey(tonic).get

  override def ascending: List[String] =
    majorScale.ascending.drop(tonicIndex) ++
      majorScale.ascending.takeRight(tonicIndex)

  override def toNotes(octave: Int): List[Note] = {
    val root = Note(ascending.head, octave).get

    for {
      op <- List(
        DiatonicInterval.unison(_),
        DiatonicInterval.second(_),
        DiatonicInterval.third(_),
        DiatonicInterval.fourth(_),
        DiatonicInterval.fifth(_),
        DiatonicInterval.sixth(_),
        DiatonicInterval.seventh(_)
      )
    } yield op(root).get
  }
}
