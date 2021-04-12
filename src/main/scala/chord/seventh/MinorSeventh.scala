package chord.seventh
import chord.triad.MinorTriad
import key.{Key, MinorKey}

/**
  * Represents a minor seventh chord.
  */
case class MinorSeventh private (tonic: String) extends Seventh {
  private val triad = MinorTriad(tonic).get

  override protected val key: Key = MinorKey(tonic).get

  override val third: String = triad.third

  override val fifth: String = triad.fifth

  override val seventh: String = key.degrees(6)
}

object MinorSeventh {
  def apply(tonic: String): Option[MinorSeventh] =
    if (MinorKey(tonic).isDefined) Some(new MinorSeventh(tonic)) else None
}
