package mode
import note.Note
import scale.MajorScale
import util.ConstructorUtils

/**
  * Represents the Ionian mode, which is effectively the major scale.
  */
case class Ionian(tonic: String) extends Mode {
  private val majorScale = MajorScale(tonic).get

  override def ascending: List[String] = majorScale.ascending

  override def toNotes(octave: Int): List[Note] = majorScale.toNotes(octave)
}

object Ionian {
  def apply(tonic: String): Option[Ionian] =
    ConstructorUtils.validTonicConstructor(tonic, s => new Ionian(s))
}
