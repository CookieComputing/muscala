package util
import note.Note

/**
  * Utilities for constructing objects
  */
object ConstructorUtils {

  /**
    * Helper function that takes a valid tonic and creates an object
    * @param tonic the tonic of whatever is to be constructed
    * @param f The constructor process
    * @tparam A The type of the object to be constructed
    * @return Some(object) if the tonic is a valid tonic, None otherwise
    */
  def validTonicConstructor[A](tonic: String, f: String => A): Option[A] =
    if (Note.tonicRegex.matches(tonic)) Some(f(tonic)) else None
}
