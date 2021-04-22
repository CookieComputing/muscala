package scale
import chord.seventh.{
  DominantSeventh,
  HalfDiminishedSeventh,
  MajorSeventh,
  MinorSeventh,
  Seventh
}
import chord.triad.{DiminishedTriad, MajorTriad, MinorTriad, Triad}
import interval.diatonic.DiatonicInterval
import key.MinorKey
import note.Note
import util.ConstructorUtils

/**
  * Represents the natural minor scale, which bases its scale degrees from
  * the minor key.
  */
case class NaturalMinorScale private (tonic: String) extends Scale {
  implicit private val key: MinorKey = MinorKey(tonic).get

  override def ascending: List[String] = key.degrees

  override def toNotes(octave: Int): List[Note] =
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
      root <- Note(tonic, octave)
    } yield op(root).get

  /**
    * Returns the triads that can be formed when starting at each scale
    * degree. The index of each triad in the list corresponds to the scale
    * degree used as the tonic of the triad.
    * @return the list of triads for the scale
    */
  def triads: List[Triad] =
    (ascending zip List(
      MinorTriad(_),
      DiminishedTriad(_),
      MajorTriad(_),
      MinorTriad(_),
      MinorTriad(_),
      MajorTriad(_),
      MajorTriad(_)
    )).map(tup => tup._2(tup._1)).map(_.get)

  /**
    * Returns the sevenths that can be formed when starting at each scale
    * degree. The index of each seventh in the list corresponds to the scale
    * degree used as the tonic of the seventh.
    * @return the list of sevenths for the scale
    */
  def sevenths: List[Seventh] =
    (ascending zip List(
      MinorSeventh(_),
      HalfDiminishedSeventh(_),
      MajorSeventh(_),
      MinorSeventh(_),
      MinorSeventh(_),
      MajorSeventh(_),
      DominantSeventh(_)
    )).map(tup => tup._2(tup._1)).map(_.get)
}

object NaturalMinorScale {
  def apply(tonic: String): Option[NaturalMinorScale] =
    ConstructorUtils.validTonicConstructor(tonic, s => new NaturalMinorScale(s))
}
