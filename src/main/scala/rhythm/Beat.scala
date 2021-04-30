package rhythm

import note.Note

/**
  * A beat is a basic unit of time in music. A measure is composed of a set
  * amount of beats, which represents the passage of time and perhaps a note
  * being played at that instant.
  */
sealed trait Beat {
  // Each beat has a set duration in which it is played.
  // This follows a notation where 1 is a whole note, 2 is a half note, 4 is
  // a quarter note, etc. This is similar to how notes are represented in a
  // time signature.
  val duration: Int
}

/**
  * Represents a note being played at a specific beat.
  */
case class BeatNote(note: Note, duration: Int) extends Beat {}

/**
  * Represents a rest, a beat where a musician would not play any note.
  */
case class Rest(duration: Int) extends Beat {}
