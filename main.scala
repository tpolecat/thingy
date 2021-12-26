
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
import scala.jdk.CollectionConverters._
import scala.concurrent.ExecutionContext
import cats.effect.std
import java.nio.charset.Charset
import java.nio.file.Paths
import java.nio.file.Files
import java.io.FileNotFoundException
import java.nio.file.NoSuchFileException
import com.monovore.decline._
import com.monovore.decline.effect._
import cats.effect.ExitCode
import cats.syntax.all._

object Main extends CommandIOApp(
  name    = "librarian",
  version = "0.0.1",
  header  =
     s"""|Create a dotfile with an org's current releases and interdependencies (if any). Redirect
         |to a file or pipe straight to `dot`.
         |
         |    librarian org.tpolecat | dot -Tsvg -o stuff.svg
         |
         |To exclude root dependencies, create ~/.librarian_excludes and list one artifact (i.e.,
         |woozle-core_2.13) per line.
         |""".stripMargin.trim,
) {

  val UTF8 = Charset.forName("UTF-8")

  def excludeList: IO[Set[ModuleName]] =
    IO(sys.env.get("HOME")).flatMap {
      case None => std.Console[IO].println("Can't get $HOME, so can't find $HOME/.librarian_excludes").as(Set.empty)
      case Some(p) =>
        IO.blocking {
          Files.readAllLines(Paths.get(p, ".librarian_excludes"), UTF8).asScala.map(ModuleName(_)).toSet
        } .recover {
          case _: NoSuchFileException => Set.empty
        }
    }

  def dotFile(ns: Map[String, List[String]]): String =
    s"""|digraph {
        |  rankdir=LR
        |  ${ns.map { case (n, ns) => s"\"$n\" -> ${ns.map(s => s"\"$s\"").mkString("{", " ", "}")}" }.mkString("\n|  ") }
        |}
     """.stripMargin

  def run(org: Organization, suffix: Suffix): IO[ExitCode] =
    for {
      ex <- excludeList
      ms <- Central.allModules(org, suffix).map(_.filterNot(m => ex(m.name)))
      ds <- ms.parTraverse(Central.latestVersion).map(_.flatten)
      ns <- Central.directDependencyModuleNameStrings(ds, org)
      _  <- IO.println(dotFile(ns))
    } yield ExitCode.Success

  val org: Opts[Organization] =
    Opts.argument[String]("org").map(Organization(_))

  val suffix: Opts[Suffix] =
    Opts.option[Suffix](
      long = "suffix",
      short = "s",
      help = "Artifact suffix, including leading underscore (default is _2.13).",
    ).withDefault(Suffix.fromString("_2.13").get)

  def main: Opts[IO[ExitCode]] =
    (org, suffix).mapN(run)

}
