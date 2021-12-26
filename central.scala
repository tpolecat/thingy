
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

/** Maven central. */
object Central {

  // low-level cs complete
  private def complete(input: String): IO[List[String]] =
    IO.fromFuture {
      IO {
        Complete()
          .withRepositories(Seq(Repositories.central))
          .withInput(input)
          .result()
          .value(ExecutionContext.global) // Future[Complete.Result]
      }
    } .flatMap(_.results.map(_._2).toList.flatTraverse(_.map(_.toList).liftTo[IO])) // IO[List[String]]

  /** All modules under `org` matching `suffix` */
  def allModules(org: Organization, suffix: Suffix): IO[List[Module]] =
    complete(s"${org.value}:")
      .map(
        _.filter(_.dropWhile(_ != '_') == suffix.value)
        .map(ModuleName(_))
        .map(Module(org, _))
      )

  /** All versions, sorted in ascending order (i.e., the last one is the most recent). */
  def allVersions(module: Module): IO[List[Version]] =
    complete(s"$module:")
      .map(_.flatMap(Version.parse).sorted) // IO[List[Version]]

  /** Dependency for the latest version of `module`, if any. */
  def latestVersion(module: Module): IO[Option[Dependency]] =
    allVersions(module).map(_.lastOption.map(v => Dependency(module, v.toString)))

  /** Resolve things. */
  def resolve(deps: List[Dependency]): IO[Resolution] =
    IO.fromFuture {
      IO {
        Resolve()
          .addDependencies(deps: _*)
          .future()(ExecutionContext.global)
      }
    }

  /** Direct dependencies. */
  def directDependencies(deps: List[Dependency]): IO[Map[Dependency, List[Dependency]]] =
    resolve(deps).map { r =>
      r.rootDependencies.map(d => (d, r.finalDependenciesCache.find(_._1.module == d.module).toList.flatMap(_._2.toList))).toMap
    }

  /** Direct deps as module + version, filtered by org. */
  def directDependencyModules(deps: List[Dependency], org: Organization): IO[Map[(Module, String), List[(Module, String)]]] =
    directDependencies(deps).map(_.map { case (k, v) => (k.module, k.version) -> v.filter(_.module.organization == org).map(d => (d.module, d.version)) })

  /** Direct deps as module name + version, filtered by org. */
  def directDependencyModuleNames(deps: List[Dependency], org: Organization): IO[Map[(ModuleName, String), List[(ModuleName, String)]]] =
    directDependencyModules(deps, org).map(_.map { case (k, v) => (k._1.name, k._2) -> v.map(d => (d._1.name, d._2)) })

  /** Direct module name strings (name:version), filtered by org. */
  def directDependencyModuleNameStrings(deps: List[Dependency], org: Organization): IO[Map[String, List[String]]] =
    directDependencyModuleNames(deps, org).map(_.map { case (k, v) => s"${k._1.value}:${k._2}" -> v.map { case (n, v) => s"${n.value}:$v" }})

}