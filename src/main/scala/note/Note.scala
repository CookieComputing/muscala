package note
import note.Note.Rank

import scala.util.matching.Regex

/**
  * A note represents the music note used in western music, containing
  * a rank which represents the equivalent pitch that a note may be translated
  * to in a physical setting. Each increment/decrement in the rank is equivalent
  * to a half step. A rank of 0 denotes "C-0", a rank of 1 denotes "C#-0",
  * a rank of 2 denotes "D-0", and so on. A negative rank is theoretically
  * possible, but would likely not be of any use to a musician.
  *
  * A note's name is the combination of a letter (A-G), followed by some number
  * of accidentals. We allow for an arbitrary amount of accidentals to be
  * appended to a name to express the possibilities of multiple accidentals.
  * Thus, operations like sharp() or flat() will simply append an accidental to
  * the note's name.
  */
case class Note private (name: String, rank: Rank) {

  /**
    * Returns this note, but any excess accidentals in its name are removed.
    * This converts double accidentals into a whole step, and any
    * sharps or flats will "cancel" each other out.
    * @return a Note with its excess accidentals removed.
    */
  def reduceAccidentals: Note = ???

  /**
    * Sharpens this note by 1 half step.
    * @return a Note with -
    */
  def sharp: Note = ???

  /**
    * Flattens this note by one half step.
    * @return
    */
  def flat: Note = ???

  /**
    *
    * @return
    */
  override def toString: String = super.toString
}

object Note {
  // 0 indicates "C-0"
  type Rank = Int

  val halfStepsInOctave = 12

  // Constants representing the symbol for sharps or flats
  val sharp: Char = '#'
  val flat: Char = 'b'

  val noteRegex: Regex = s"[A-G]($sharp|$flat)*".r

  /**
    * Creates a note. If the note name is not valid, returns None.
    * @param name A name for the note.
    * @return Some note if the note name is valid, None otherwise.
    */
  def apply(name: String): Option[Note] =
    if (noteRegex.matches(name))
      Some(Note(name, nameToRank(name)))
    else None

  // Internal mapping of name to a rank, assumes noteRegex matches
  private def nameToRank(name: String): Rank =
    name.drop(1).foldLeft(letterToRank(name.head)) { (rank: Int, char: Char) =>
      char match {
        case Note.sharp => rank + 1
        case Note.flat  => rank - 1
      }
    }

  // C marks the start of the octave
  private val letterToRank = Map(
    'C' -> 0,
    'D' -> 2,
    'E' -> 4,
    'F' -> 5,
    'G' -> 7,
    'A' -> 9,
    'B' -> 11
  )
}
