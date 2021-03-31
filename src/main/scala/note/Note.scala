package note
import note.Note.{
  Rank,
  accidentalToDelta,
  halfStepsInOctave,
  letterToRank,
  newAccidentalName,
  rankToLetter
}

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
    * Returns this note, but any conflicting accidentals in its name are
    * removed. Any sharps or flats will "cancel" each other out.
    * @return a Note with its conflicting accidentals removed.
    */
  def clearConflictingAccidentals: Note = {
    val accidentalCount = name
      .drop(1)
      .foldLeft(0)((acc, c) => acc + accidentalToDelta(c))
    val accidental = {
      if (accidentalCount > 0) Note.sharp else Note.flat
    }
    new Note(newAccidentalName(name.head, accidental, accidentalCount), rank)
  }

  /**
    * Converts a note's name to the closest note after factoring the
    * accidentals, for instance "B#" will become "C", while "Dbb" will become
    * "C". If there is still is an accidental, it will remain after the
    * operation.
    * @return a note with a name that has been converted to the nearest note.
    */
  def nearestNote: Note = {
    val clearedNote = clearConflictingAccidentals

    val (newLetter, leftOverAccidentals) =
      clearedNote.name.drop(1).foldLeft((name.head, 0)) {
        case ((letter: Char, acc: Int), char: Char) =>
          val delta = accidentalToDelta(char)
          val newRank = math.floorMod(letterToRank(letter) + acc + delta,
                                      Note.halfStepsInOctave)
          if (rankToLetter.contains(newRank))
            (rankToLetter(newRank), 0)
          else
            (letter, acc + delta)
      }
    val accidental = if (leftOverAccidentals > 0) Note.sharp else Note.flat
    new Note(newAccidentalName(newLetter, accidental, leftOverAccidentals),
             rank)
  }

  /**
    * Sharpens this note by 1 half step.
    * @return a sharpened Note
    */
  def sharp: Note = new Note(name + Note.sharp, rank + 1)

  /**
    * Flattens this note by one half step.
    * @return
    */
  def flat: Note = new Note(name + Note.flat, rank - 1)

  /**
    *
    * @return
    */
  override def toString: String = s"$name-$octave"

  /**
    * Returns the octave for this note. Edge cases like "B#-0" will spill into
    * the next octave
    * @return The octave of this note.
    */
  def octave: Int = math.floorDiv(rank, halfStepsInOctave)
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
    * Creates a note in the 4th octave. If the note name is not valid, returns
    * None.
    * @param name A name for the note.
    * @return Some note if the note name is valid, None otherwise.
    */
  def apply(name: String): Option[Note] = Note(name, 4)

  /**
    * Creates a note. If the note name is not valid, returns None.
    * @param name A name for the note.
    * @param octave The octave of the note.
    * @return Some note if the note name is valid, None otherwise.
    */
  def apply(name: String, octave: Int): Option[Note] = {
    if (noteRegex.matches(name)) {
      Some(new Note(name, octave * halfStepsInOctave + nameToRank(name)))
    } else None
  }

  // Internal mapping of name to a rank, assumes noteRegex matches
  private def nameToRank(name: String): Rank =
    name.drop(1).foldLeft(letterToRank(name.head)) { (rank: Int, char: Char) =>
      rank + accidentalToDelta(char)
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

  private val rankToLetter = for ((k, v) <- letterToRank) yield (v, k)
  private def accidentalToDelta(char: Char) = char match {
    case Note.sharp => 1
    case Note.flat  => -1
  }

  // Helper to perform absolute value on the count of accidentals
  private def newAccidentalName(letter: Char, accidental: Char, times: Int) =
    letter.toString + (accidental.toString * math.abs(times))
}
