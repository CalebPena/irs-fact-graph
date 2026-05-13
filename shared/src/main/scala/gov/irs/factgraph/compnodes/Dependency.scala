package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ FactDefinition, FactDictionary, Factual, Path, RecursiveDependency }
import gov.irs.factgraph.definitions.fact.{ CommonOptionConfigTraits, CompNodeConfigTrait }
import gov.irs.factgraph.monads.Result

object Dependency extends CompNodeFactory:
  override val Key: String = "Dependency"

  def apply(path: Path)(using fact: Factual): CompNode =
    val (startFact, resolvedPath) = path.popEscapes(fact)

    // Self-referential Dependency built *during* the host's `value` lazy
    // val init: walk the path through the dictionary (no value access) and
    // return a typed stub. The lazy-val-in-progress flag means going
    // through the normal `startFact.apply` path would re-enter the same
    // lazy val and deadlock on its init latch. Tests that build CompNodes
    // outside any lazy val init (no `valueInProgress` entry) still take
    // the normal path even when the path happens to point back at the
    // host fact.
    fact match
      case host: FactDefinition
          if (startFact eq host) &&
            host.dictionary.valueInProgress.contains(host.path) =>
        RecursiveDependency.resolveAbstractPath(host, resolvedPath) match
          case Some(resolved) if resolved == host.path =>
            return RecursiveDependency
              .inferType(host)
              .flatMap(RecursiveDependency.stubDependency(_, path))
              .getOrElse(
                throw new IllegalArgumentException(
                  s"recursive Dependency on '${host.path}' but its output " +
                    s"type couldn't be inferred from the XML config",
                ),
              )
          case Some(resolved) =>
            host.dictionary(resolved) match
              case Some(target) => return target.value.dependency(path)
              case None         =>
        // If the walker couldn't resolve the path, fall through to the
        // normal resolution and let it raise the standard error.
          case None =>
      case _ =>

    startFact(resolvedPath)(0) match
      // Stored path is the original (un-popped) one so runtime re-pops
      // against each member's selfStack.
      case Result.Complete(target) => target.value.dependency(path)
      case _                       =>
        throw new IllegalArgumentException(
          s"cannot find fact at path '$path' from '${fact.path}'",
        )

  override def fromDerivedConfig(e: CompNodeConfigTrait)(using
      Factual,
  )(using
      FactDictionary,
  ): CompNode =
    this(Path(e.getOptionValue(CommonOptionConfigTraits.PATH).get))
