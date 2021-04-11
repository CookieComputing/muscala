package key
import note.Note
import note.Note.{flat, sharp}

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
  * Represents a key in western harmony.
  */
sealed trait Key {
  val tonic: String
  // For major and minor keys, there is a guarantee that there will be 7 notes.
  val degrees: List[String]

  /**
    * Returns the parallel key of this key. For instance, if this key was C
    * major, returns C minor
    * @return the parallel key of this key
    */
  def parallelKey: Key

  /**
    * Returns the dominant key of this key, which is the key whose tonic is a
    * perfect fifth above this key's tonic.
    * @return the dominant key of this key
    */
  def dominantKey: Key
}

/**
  * Represents a major key in western harmony.
  */
case class MajorKey(tonic: String) extends Key {
  override val degrees: List[String] =
    MajorKey.generateScales(MajorKey.startingKey, tonic)

  /**
    * Generates the relative minor of this key
    * @return the relative minor
    */
  def relativeMinor: MinorKey = MinorKey(degrees(5)).get

  /**
    * Returns the minor key parallel to this major key
    *  @return the parallel key of this key
    */
  override def parallelKey: Key = MinorKey(tonic).get

  /**
    * Returns the dominant key of this key
    * @return the dominant key of this key
    */
  override def dominantKey: Key = MajorKey(degrees(4)).get
}

/**
  * Represents a minor key in western harmony.
  */
case class MinorKey(tonic: String) extends Key {
  override val degrees: List[String] =
    MinorKey.generateScales(MinorKey.startingKey, tonic)

  /**
    * Generates the relative major of this key
    * @return the relative major
    */
  def relativeMajor: MajorKey = MajorKey(degrees(2)).get

  /**
    * Returns the major key parallel to this minor key
    *  @return the parallel key of this key
    */
  override def parallelKey: Key = MajorKey(tonic).get

  /**
    * Returns the dominant key of this key
    *  @return the dominant key of this key
    */
  override def dominantKey: Key = MinorKey(degrees(4)).get
}

/**
  * Constructor logic for major keys.
  */
object MajorKey extends KeyBuilder {

  /**
    * Generates a major key, if the provided tonic is a valid key
    * @param tonic a potential tonic for the key
    * @return a major key if the tonic is valid, None otherwise
    */
  def apply(tonic: String): Option[MajorKey] =
    if (tonicRegex.matches(tonic))
      Some(new MajorKey(tonic))
    else None

  // Key generation starts from the key of "C"
  private val startingKey = List("C", "D", "E", "F", "G", "A", "B")

  // Sharping a key => 5th is now the tonic, 7th of new key is sharp
  def rotateKeyClockwise(scales: List[String]): List[String] = {
    val newScale = rotateLeft(scales, 4)
    newScale.updated(6, sharpNote(newScale(6)))
  }

  // Flatting a key => 4th is now the tonic, 4th of new key is flat
  def rotateKeyCounterClockwise(scales: List[String]): List[String] = {
    val newScale = rotateLeft(scales, 3)
    newScale.updated(3, flatNote(newScale(3)))
  }
}

/**
  * Constructor logic for minor keys.
  */
object MinorKey extends KeyBuilder {

  /**
    * Generates a minor key, if the provided tonic is a valid key
    * @param tonic a potential tonic for the key
    * @return a minor key if the tonic is valid, None otherwise
    */
  def apply(tonic: String): Option[MinorKey] =
    if (tonicRegex.matches(tonic))
      Some(new MinorKey(tonic))
    else None

  // Key generation starts from the key of "C"
  private val startingKey = List("A", "B", "C", "D", "E", "F", "G")

  // Sharping a key => 5th is now the tonic, 2nd of new key is sharp
  def rotateKeyClockwise(scales: List[String]): List[String] = {
    val newScale = rotateLeft(scales, 4)
    newScale.updated(1, sharpNote(newScale(1)))
  }

  // Flatting a key => 4th is now the tonic, 6th of new key is flat
  def rotateKeyCounterClockwise(scales: List[String]): List[String] = {
    val newScale = rotateLeft(scales, 3)
    newScale.updated(5, flatNote(newScale(5)))
  }
}

/**
  * Abstract logic for major and minor key scale construction
  */
protected trait KeyBuilder {
  val tonicRegex: Regex = s"[A-G]($sharp*|$flat*)".r

  // used to determine which direction of the circle of fifths to head towards
  private val keyOrdering = Map(
    'F' -> 1,
    'C' -> 2,
    'G' -> 3,
    'D' -> 4,
    'A' -> 5,
    'E' -> 6,
    'B' -> 7
  )

  // Generation starts by following the circle of fifths and repeats until
  // the desired key has been generated
  @tailrec
  final def generateScales(startingScales: List[String],
                           tonic: String): List[String] =
    if (tonic == startingScales.head) startingScales
    else {
      val alterKey = (scales: List[String]) =>
        if (clockwise(startingScales.head, tonic)) rotateKeyClockwise(scales)
        else rotateKeyCounterClockwise(scales)
      generateScales(alterKey(startingScales), tonic)
    }

  // Determines if we should go clockwise around the circle of fifths.
  private def clockwise(currentTonic: String, desiredTonic: String): Boolean = {
    def accidentalSum(tonic: String) = tonic.drop(1).foldLeft(0) {
      (acc: Int, c: Char) =>
        c match {
          case Note.sharp => acc + 1
          case Note.flat  => acc - 1
        }
    }
    val currentTonicAccidentals = accidentalSum(currentTonic)
    val desiredTonicAccidentals = accidentalSum(desiredTonic)

    if (currentTonicAccidentals < desiredTonicAccidentals) true
    else if (currentTonicAccidentals > desiredTonicAccidentals) false
    else keyOrdering(currentTonic.head) < keyOrdering(desiredTonic.head)
  }

  // Given a list of scale degrees, returns the next scale degrees that should
  // be expected on the circle of fifths when rotating clockwise
  def rotateKeyClockwise(scales: List[String]): List[String]

  // Given a list of scale degrees, returns the next scale degrees that should
  // be expected on the circle of fifths when rotating counterclockwise
  def rotateKeyCounterClockwise(scales: List[String]): List[String]

  // Rotates the list left such that the pivot is now at the front of the list.
  def rotateLeft(scales: List[String], pivot: Int): List[String] =
    scales.drop(pivot) ++ scales.take(pivot)

  def sharpNote(note: String): String =
    if (note.last == Note.flat)
      note.dropRight(1)
    else note + Note.sharp.toString

  def flatNote(note: String): String =
    if (note.last == Note.sharp)
      note.dropRight(1)
    else note + Note.flat.toString
}
