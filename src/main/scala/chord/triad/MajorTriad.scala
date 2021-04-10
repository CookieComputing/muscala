package chord.triad
import interval.diatonic.DiatonicInterval
import key.MajorKey
import note.Note

case class MajorTriad private (tonic: String) extends Triad {
  implicit private val key: MajorKey = MajorKey(tonic).get

  override val tones: List[String] = List(
    key.degrees.head,
    key.degrees(2),
    key.degrees(4)
  )

  override val third: String = tones(1)
  override val fifth: String = tones(2)

  override def toNotes(octave: Int): List[Note] = {
    val root = Note(tonic, octave).get
    List(
      DiatonicInterval.unison(root),
      DiatonicInterval.third(root),
      DiatonicInterval.fifth(root)
    ).map(_.get)
  }
}

object MajorTriad {
  def apply(tonic: String): Option[MajorTriad] =
    if (MajorKey(tonic).isDefined) Some(new MajorTriad(tonic)) else None

}
