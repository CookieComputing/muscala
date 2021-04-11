package interval.diatonic
import key.Key
import note.Note

import scala.annotation.tailrec

/**
  * Provides utility methods for working with intervals at a diatonic level. This
  * differs from the absolute intervals module by providing matching the names
  * of an interval to the scale degree that would be expected in a given key.
  * For instance, if a diatonic third was requested for the key of "C" for a
  * note that is enharmonic to "E", we would expect "G". This module does not
  * work for notes that do not fit in the key, such as accidentals in the key.
  */
object DiatonicInterval {

  /**
    * Represents a note a unison interval away from this note
    * @param note the note to base the diatonic interval from
    * @param key the key to extract diatonic intervals from
    * @return a note a unison interval away, if the given note is enharmonic
    *         with a note from the key, else None.
    */
  def unison(note: Note)(implicit key: Key): Option[Note] = move(note, 0, key)

  /**
    * Represents a note a second interval away from this note
    * @param note the note to base the diatonic interval from
    * @param key the key to extract diatonic intervals from
    * @return a note a second interval away, if the given note is enharmonic
    *         with a note from the key, else None.
    */
  def second(note: Note)(implicit key: Key): Option[Note] = move(note, 1, key)

  /**
    * Represents a note a third interval away from this note
    * @param note the note to base the diatonic interval from
    * @param key the key to extract diatonic intervals from
    * @return a note a third interval away, if the given note is enharmonic
    *         with a note from the key, else None.
    */
  def third(note: Note)(implicit key: Key): Option[Note] = move(note, 2, key)

  /**
    * Represents a note a fourth interval away from this note
    * @param note the note to base the diatonic interval from
    * @param key the key to extract diatonic intervals from
    * @return a note a fourth interval away, if the given note is enharmonic
    *         with a note from the key, else None.
    */
  def fourth(note: Note)(implicit key: Key): Option[Note] = move(note, 3, key)

  /**
    * Represents a note a fifth interval away from this note
    * @param note the note to base the diatonic interval from
    * @param key the key to extract diatonic intervals from
    * @return a note a fifth interval away, if the given note is enharmonic
    *         with a note from the key, else None.
    */
  def fifth(note: Note)(implicit key: Key): Option[Note] = move(note, 4, key)

  /**
    * Represents a note a sixth interval away from this note
    * @param note the note to base the diatonic interval from
    * @param key the key to extract diatonic intervals from
    * @return a note a sixth interval away, if the given note is enharmonic
    *         with a note from the key, else None.
    */
  def sixth(note: Note)(implicit key: Key): Option[Note] = move(note, 5, key)

  /**
    * Represents a note a seventh interval away from this note
    * @param note the note to base the diatonic interval from
    * @param key the key to extract diatonic intervals from
    * @return a note a seventh interval away, if the given note is enharmonic
    *         with a note from the key, else None.
    */
  def seventh(note: Note)(implicit key: Key): Option[Note] = move(note, 6, key)

  /**
    * Represents a note a octave interval away from this note
    * @param note the note to base the diatonic interval from
    * @param key the key to extract diatonic intervals from
    * @return a note a octave interval away, if the given note is enharmonic
    *         with a note from the key, else None.
    */
  def octave(note: Note)(implicit key: Key): Option[Note] =
    move(note, 7, key).flatMap(n => Note(n.name, n.octave + 1))

  /**
    * Helper to perform the actual movement from a note to another note in
    * the key. If the note does not exist in the key before moving or after
    * movement, return None
    * @param note the root note to base the diatonic interval from
    * @param interval how far the desired note is from the given note
    * @param key the key to extract diatonic intervals from
    * @return a note if the given note is enharmonic, None otherwise
    */
  private def move(note: Note, interval: Int, key: Key): Option[Note] =
    if (inKey(note, key)) {
      val origin = key.degrees
        .map(Note(_).get.nearestNote)
        .indexWhere(n => n.name == note.nearestNote.name)

      val dest = Math.floorMod(origin + interval, key.degrees.size)
      val destNote = Note(key.degrees(dest)).get
      Some(sharpUntilSimilar(note, destNote).copy(name = key.degrees(dest)))
    } else None

  // Helper to check if a note is in key
  private def inKey(note: Note, key: Key): Boolean =
    key.degrees
      .map(Note(_).get.nearestNote)
      .exists(_.name == note.nearestNote.name)

  // Sharp a note until the note matches the desired note string
  @tailrec
  private def sharpUntilSimilar(note: Note, goal: Note): Note =
    if (Math.floorMod(note.rank, Note.halfStepsInOctave) == Math.floorMod(
          goal.rank,
          Note.halfStepsInOctave))
      note
    else
      sharpUntilSimilar(note.sharp, goal)
}
