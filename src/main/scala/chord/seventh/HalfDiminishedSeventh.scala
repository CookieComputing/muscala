package chord.seventh
import chord.triad.DiminishedTriad
import interval.diatonic.DiatonicInterval
import key.{Key, MinorKey}
import note.Note
import util.ConstructorUtils

/**
  * Represents a half diminished seventh, which is a diminished triad with a
  * minor seventh.
  */
case class HalfDiminishedSeventh private (tonic: String) extends Seventh {
  private val triad = DiminishedTriad(tonic).get

  override protected val key: Key = MinorKey(tonic).get

  override val third: String = triad.third

  override val fifth: String = triad.fifth

  override val seventh: String = key.degrees(6)

  override def toNotes(octave: Int): List[Note] = {
    val originalNotes = super.toNotes(octave)

    originalNotes.updated(2,
                          DiatonicInterval
                            .fifth(originalNotes.head)(key)
                            .get
                            .flat
                            .clearConflictingAccidentals)
  }
}

object HalfDiminishedSeventh {
  def apply(tonic: String): Option[HalfDiminishedSeventh] =
    ConstructorUtils.validTonicConstructor(tonic,
                                           s => new HalfDiminishedSeventh(s))
}
