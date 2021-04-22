package chord.seventh
import chord.triad.MajorTriad
import key.{Key, MajorKey}
import util.ConstructorUtils

/**
  * Represents a major seventh chord.
  */
case class MajorSeventh private (tonic: String) extends Seventh {
  private val triad = MajorTriad(tonic).get

  override protected val key: Key = MajorKey(tonic).get

  override val third: String = triad.third

  override val fifth: String = triad.fifth

  override val seventh: String = key.degrees(6)
}

object MajorSeventh {
  def apply(tonic: String): Option[MajorSeventh] =
    ConstructorUtils.validTonicConstructor(tonic, s => new MajorSeventh(s))
}
