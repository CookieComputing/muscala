package note
import interval.{
  AugmentedInterval,
  DiminishedInterval,
  MajorInterval,
  MinorInterval,
  PerfectInterval,
  TritoneInterval
}
import note.Note.{
  Rank,
  accidentFromCount,
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
    * Creates a MajorInterval intermediary movement for this note.
    * @return A MajorInterval intermediary movement
    */
  def major: MajorInterval = MajorInterval(this)

  /**
    * Creates a MinorInterval intermediary movement for this note.
    * @return A MinorInterval intermediary movement
    */
  def minor: MinorInterval = MinorInterval(this)

  /**
    * Creates a PerfectInterval intermediary movement for this note.
    * @return A PerfectInterval intermediary movement
    */
  def perfect: PerfectInterval = PerfectInterval(this)

  /**
    * Creates an AugmentedInterval intermediary movement for this note.
    * @return An AugmentedInterval intermediary movement
    */
  def augmented: AugmentedInterval = AugmentedInterval(this)

  /**
    * Creates a Diminished intermediary movement for this note.
    * @return A Diminished intermediary movement
    */
  def diminished: DiminishedInterval = DiminishedInterval(this)

  /**
    * Creates a tritone away from this note.
    * @return A tritone
    */
  def tritone: Note = TritoneInterval(this).tritoneInterval

  /**
    * Returns this note, but any conflicting accidentals in its name are
    * removed. Any sharps or flats will "cancel" each other out.
    * @return a Note with its conflicting accidentals removed.
    */
  def clearConflictingAccidentals: Note = {
    val accidentalCount = accidentals.count(_ == Note.sharp) - name
      .drop(1)
      .count(_ == Note.flat)
    new Note(newAccidentalName(letter,
                               accidentFromCount(accidentalCount),
                               accidentalCount),
             rank)
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
      clearedNote.accidentals.foldLeft((letter, 0)) {
        case ((letter: Char, acc: Int), char: Char) =>
          val delta = accidentalToDelta(char)
          val newRank = math.floorMod(letterToRank(letter) + acc + delta,
                                      Note.halfStepsInOctave)
          if (rankToLetter.contains(newRank))
            (rankToLetter(newRank), 0)
          else
            (letter, acc + delta)
      }
    new Note(newAccidentalName(newLetter,
                               accidentFromCount(leftOverAccidentals),
                               leftOverAccidentals),
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
    * Returns the note's letter, for instance the Note "C#-4" will return 'C'.
    * @return The note's letter
    */
  def letter: Char = name.head

  /**
    * Returns the accidentals of the note. for instance "C#-4" will return "#".
    * @return The note's accidentals
    */
  def accidentals: String = name.drop(1)

  /**
    * Returns a string version of this note
    * @return A string containing the name of the note and its octave
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

  /**
    * Calculates the interval distance between two notes. The comparison is done
    * by comparing the distance from the "fromNote" to the "toNote": Positive
    * distances mean that a fromNote is below the toNote, and the opposite holds
    * true if the distance is negative. A distance of 0 means they are
    * enharmonic.
    * @param fromNote the base Note to compare
    * @param toNote the target Note
    * @return the distance between the two notes
    */
  def distance(fromNote: Note, toNote: Note): Int = toNote.rank - fromNote.rank

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

  private def accidentalToDelta(char: Char) = char match {
    case Note.sharp => 1
    case Note.flat  => -1
  }
  private val rankToLetter = for ((k, v) <- letterToRank) yield (v, k)

  // Helper to perform absolute value on the count of accidentals
  private def newAccidentalName(letter: Char, accidental: Char, times: Int) =
    letter.toString + (accidental.toString * math.abs(times))

  // Helper to determine if the accidental to be used should be sharp or flat
  private def accidentFromCount(count: Int) =
    if (count > 0) Note.sharp
    else
      Note.flat
}
