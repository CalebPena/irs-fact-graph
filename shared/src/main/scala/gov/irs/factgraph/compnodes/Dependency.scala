package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ FactDefinition, FactDictionary, Factual, Path, RecursiveDependency }
import gov.irs.factgraph.definitions.fact.{ CommonOptionConfigTraits, CompNodeConfigTrait }
import gov.irs.factgraph.monads.Result

object Dependency extends CompNodeFactory:
  override val Key: String = "Dependency"

  def apply(path: Path)(using fact: Factual): CompNode =
    val (startFact, resolvedPath) = path.popEscapes(fact)

    // When the host's `value` lazy val is currently initializing, resolving
    // the path through `host.apply` / `target.value` would re-enter a lazy
    // val (the host itself for direct self-reference, or some other
    // FactDefinition for mutual recursion that loops back into this build
    // chain). Walk the path through the dictionary to detect both cases
    // and emit a typed stub whose type is inferred from the would-be
    // target's stored config.
    fact match
      case host: FactDefinition
          if (startFact eq host) &&
            host.dictionary.valueInProgress.contains(host.path) =>
        RecursiveDependency.resolveAbstractPath(host, resolvedPath) match
          case Some(resolved) =>
            host.dictionary(resolved) match
              case Some(target)
                  if target.dictionary.valueInProgress.contains(target.path) =>
                // Direct self-reference or mutual recursion — target's
                // own value is mid-init. Build a typed stub from the
                // target's static config so we never touch its `value`.
                return RecursiveDependency
                  .inferType(target)
                  .flatMap(RecursiveDependency.stubDependency(_, path))
                  .getOrElse(
                    throw new IllegalArgumentException(
                      s"recursive Dependency on '${target.path}' but its " +
                        s"output type couldn't be inferred from the XML config",
                    ),
                  )
              case Some(target) =>
                return target.value.dependency(path)
              case None =>
                // Walker resolved to a path that isn't in the dictionary —
                // fall through and let the normal resolution raise the
                // standard "cannot find fact" error.
          case None =>
      case _ =>

    startFact(resolvedPath)(0) match
      // Stored path is the original (un-popped) one so runtime re-pops
      // against each member's selfStack.
      case Result.Complete(target) =>
        // Mutual recursion guard: any target whose value lazy val is also
        // mid-init gets a typed stub from its config rather than a
        // lazy-val-deadlocking `target.value` access. Covers cycles that
        // traverse Filter predicates (where `fact` is a WithSelfStack
        // wrapper and the FactDefinition-keyed pre-check above doesn't
        // trigger).
        target match
          case fd: FactDefinition
              if fd.dictionary.valueInProgress.contains(fd.path) =>
            RecursiveDependency
              .inferType(fd)
              .flatMap(RecursiveDependency.stubDependency(_, path))
              .getOrElse(
                throw new IllegalArgumentException(
                  s"recursive Dependency on '${fd.path}' but its output " +
                    s"type couldn't be inferred from the XML config",
                ),
              )
          case _ => target.value.dependency(path)
      case _ =>
        throw new IllegalArgumentException(
          s"cannot find fact at path '$path' from '${fact.path}'",
        )

  override def fromDerivedConfig(e: CompNodeConfigTrait)(using
      Factual,
  )(using
      FactDictionary,
  ): CompNode =
    this(Path(e.getOptionValue(CommonOptionConfigTraits.PATH).get))
