
// A version of the form 1.2.3-M4, 1.2.3-RC4, or 1.2.3, ordered as expected
sealed trait Version {
  import Version._

  def format: String =
    this match {
      case Release(ma, mi, pa)              => s"$ma.$mi.$pa"
      case Milestone(ma, mi, pa, m)         => s"$ma.$mi.$pa-M$m"
      case ReleaseCandidate(ma, mi, pa, rc) => s"$ma.$mi.$pa-RC$rc"
    }

  final override def toString =
    format

}

object Version {

  private case class Release(major: Int, minor: Int, patch: Int)                   extends Version
  private case class Milestone(major: Int, minor: Int, patch: Int, milestone: Int) extends Version
  private case class ReleaseCandidate(major: Int, minor: Int, patch: Int, rc: Int) extends Version

  private val ReleasePat          = "^(\\d+)\\.(\\d+)\\.(\\d+)$".r
  private val MilestonePat        = "^(\\d+)\\.(\\d+)\\.(\\d+)-M(\\d+)$".r
  private val ReleaseCandidatePat = "^(\\d+)\\.(\\d+)\\.(\\d+)-RC(\\d+)$".r

  def parse(s: String): Option[Version] =
    s match {
      case ReleasePat(ma, mi, pa)              => Some(Release(ma.toInt, mi.toInt, pa.toInt))
      case MilestonePat(ma, mi, pa, m)         => Some(Milestone(ma.toInt, mi.toInt, pa.toInt, m.toInt))
      case ReleaseCandidatePat(ma, mi, pa, rc) => Some(ReleaseCandidate(ma.toInt, mi.toInt, pa.toInt, rc.toInt))
      case _                                   => None
    }

  implicit val OrderingVersion: Ordering[Version] =
    Ordering.by {
      case Release(ma, mi, pa)              => (ma, mi, pa, 3, 0)
      case ReleaseCandidate(ma, mi, pa, rc) => (ma, mi, pa, 2, rc)
      case Milestone(ma, mi, pa, m)         => (ma, mi, pa, 1, m)
    }

}


