package key
import key.MinorKey.generateScales
import note.Note
import note.Note.{flat, sharp}

import scala.annotation.tailrec
import scala.util.matching.Regex

/**
  * Represents a minor key in western harmony.
  */
case class MinorKey(tonic: String) {
  val degrees: List[String] = generateScales(MinorKey.startingKey, tonic)
}

object MinorKey {

  /**
    * Generates a minor key, if the provided tonic is a valid key
    * @param tonic a potential tonic for the key
    * @return a minor key if the tonic is valid, None otherwise
    */
  def apply(tonic: String): Option[MinorKey] =
    if (tonicRegex.matches(tonic))
      Some(new MinorKey(tonic))
    else None

  val tonicRegex: Regex = s"[A-G]($sharp*|$flat*)".r

  // Key generation starts from the key of "C"
  private val startingKey = List("A", "B", "C", "D", "E", "F", "G")

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
  private def generateScales(startingScales: List[String],
                             tonic: String): List[String] =
    if (tonic == startingScales.head) startingScales
    else {
      val alterKey = (scales: List[String]) =>
        if (clockwise(startingScales.head, tonic)) sharpKey(scales)
        else flatKey(scales)
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

  // Sharping a key => 5th is now the tonic, 2nd of new key is sharp
  private def sharpKey(scales: List[String]): List[String] = {
    val newScale = rotateLeft(scales, 4)
    newScale.updated(1, sharpNote(newScale(1)))
  }

  // Flatting a key => 4th is now the tonic, 6th of new key is flat
  private def flatKey(scales: List[String]): List[String] = {
    val newScale = rotateLeft(scales, 3)
    newScale.updated(5, flatNote(newScale(5)))
  }

  // Rotates the list left such that the pivot is now at the front of the list.
  private def rotateLeft(scales: List[String], pivot: Int): List[String] =
    scales.drop(pivot) ++ scales.take(pivot)

  private def sharpNote(note: String): String =
    if (note.last == Note.flat)
      note.dropRight(1)
    else note + Note.sharp.toString

  private def flatNote(note: String): String =
    if (note.last == Note.sharp)
      note.dropRight(1)
    else note + Note.flat.toString
}
