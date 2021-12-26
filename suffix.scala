import com.monovore.decline.Argument
import cats.syntax.all._

/** A suffix's value always begins with an underscore. */
sealed abstract case class Suffix(value: String)

object Suffix {

  def fromString(s: String): Option[Suffix] =
    Some(s).filter(_.startsWith("_")).map(new Suffix(_) {})

  implicit val ArgumentSuffix: Argument[Suffix] =
    Argument.from("suffix") { s =>
      fromString(s) match {
        case Some(v) => v.validNel
        case None    => "Suffix must begin with an underscore (like _2.13 for example).".invalidNel
      }
    }

}

