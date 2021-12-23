
import cats.effect.IO
import cats.effect.IOApp
import cats.effect.kernel.Clock
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import coursier._
import coursier.complete.Complete
import coursier.parse.CachePolicyParser
import coursier.parse.RepositoryParser
import coursier.util.Task

import scala.concurrent.ExecutionContext

object Main extends IOApp.Simple {

  // Stuff specific to me
  val org = Organization("org.tpolecat")
  val scalaVersion = "2.13"
  val artifactPrefixes = List(
      "atto-core",
      "doobie-core",
      "natchez-core",
      "natchez-http4s",
      "skunk-core",
      "pool-party",
      "sourcepos",
      "typename",
    )

  // The above information as Coursier Modules
  def modules: List[Module] =
    artifactPrefixes
      .map(s => ModuleName(s"${s}_$scalaVersion"))
      .map(n => Module(org, n, Map.empty))

  def run: IO[Unit] =
    for {
      ds <- modules.parTraverse(Central.latestVersion)
      ns <- Central.directDependencyModuleNameStrings(ds, org)
      _  <- IO.println("digraph {")
      _  <- ns.toList.traverse { case (n, ns) => IO.println(s"  \"$n\" -> ${ns.map(s => s"\"$s\"").mkString("{", " ", "}")}") }
      _  <- IO.println("}")
    } yield ()

}
