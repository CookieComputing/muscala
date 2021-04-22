package chord.seventh
import chord.triad.DiminishedTriad
import interval.diatonic.DiatonicInterval
import key.{Key, MinorKey}
import note.Note
import util.ConstructorUtils

/**
  * Represents a diminished seventh chord.
  */
case class DiminishedSeventh private (tonic: String) extends Seventh {
  private val triad = DiminishedTriad(tonic).get

  override protected val key: Key = MinorKey(tonic).get

  override val third: String = triad.third

  override val fifth: String = triad.fifth

  override val seventh: String = Note.flatName(key.degrees(6))

  override def toNotes(octave: Int): List[Note] = {
    val originalNotes = super.toNotes(octave)
    originalNotes.dropRight(2) ++ List(
      DiatonicInterval
        .fifth(originalNotes.head)(key)
        .get
        .flat
        .clearConflictingAccidentals,
      DiatonicInterval
        .seventh(originalNotes.head)(key)
        .get
        .flat
        .clearConflictingAccidentals
    )
  }
}

object DiminishedSeventh {
  def apply(tonic: String): Option[DiminishedSeventh] =
    ConstructorUtils.validTonicConstructor(tonic, s => new DiminishedSeventh(s))
}
