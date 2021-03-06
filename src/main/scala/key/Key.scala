package key
import note.Note
import scale.{MajorScale, NaturalMinorScale}
import util.ConstructorUtils

import scala.annotation.tailrec

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

  /**
    * Returns the subdominant key of this key, which is the key whose tonic is
    * a perfect fourth above this key's tonic
    * @return
    */
  def subdominantKey: Key

  /**
    * Determines if a key is a theoretical key. That is, if any note in the
    * key signature has a double (or greater!) accidental, it's generally
    * impractical to read and is not commonly used in pieces.
    */
  def isTheoreticalKey: Boolean =
    degrees.exists(note =>
      note.count(c => c == Note.flat || c == Note.sharp) > 1)

  /**
    * Returns the key signature of this key. That is, any note that is not a
    * natural note will be returned. For theoretical keys, this is largely
    * useless as it effectively just return the degrees of the key.
    * @return any notes in the key that contain accidentals
    */
  def signature: List[String] =
    degrees.filter(s => s.contains(Note.flat) || s.contains(Note.sharp))
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

  /**
    * Returns the subdominant key of this key
    *  @return the subdominant key of this key
    */
  override def subdominantKey: Key = MajorKey(degrees(3)).get

  /**
    * Returns the major scale starting with this key's tonic
    * @return the major scale
    */
  def scale: MajorScale = MajorScale(tonic).get
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

  /**
    * Returns the subdominant key of this key
    *  @return the subdominant key of this key
    */
  override def subdominantKey: Key = MinorKey(degrees(3)).get

  /**
    * Returns the natural minor scale starting with this key's tonic
    * @return the natural minor scale
    */
  def naturalMinorScale: NaturalMinorScale = NaturalMinorScale(tonic).get
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
    ConstructorUtils.validTonicConstructor(tonic, s => new MajorKey(s))

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
    ConstructorUtils.validTonicConstructor(tonic, s => new MinorKey(s))

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
  // Generation starts by following the circle of fifths and repeats until
  // the desired key has been generated
  @tailrec
  final def generateScales(startingScales: List[String],
                           tonic: String): List[String] =
    KeyNavigator.directionTowards(startingScales, tonic, 0) match {
      case Direction.Clockwise =>
        generateScales(rotateKeyClockwise(startingScales), tonic)
      case Direction.CounterClockwise =>
        generateScales(rotateKeyCounterClockwise(startingScales), tonic)
      case Direction.None => startingScales
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
