package rhythm

/**
  * A measure is a collection of beats representing a compositional music
  * block which contains a set number of beats defined by the time signature
  * of a piece.
  */
case class Measure(beats: List[Beat]) {}
