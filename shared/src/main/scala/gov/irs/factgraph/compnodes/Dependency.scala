package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ FactDictionary, Factual, Path }
import gov.irs.factgraph.definitions.fact.{ CommonOptionConfigTraits, CompNodeConfigTrait }
import gov.irs.factgraph.monads.Result

object Dependency extends CompNodeFactory:
  override val Key: String = "Dependency"

  /** Build a Dependency CompNode targeting the fact at `path` relative to
    * `fact`. If `path` starts with `^` Escape items, they're resolved
    * against `fact.selfStack` — the chain of enclosing collection-item
    * scopes Filter / IndexOf / Find have pushed (empty by default for
    * top-level facts).
    *
    * The stored Expression.Dependency keeps the original `path` (escape
    * items and all) so runtime evaluation can re-resolve against whatever
    * self-stack is in scope when the Filter's predicate actually fires —
    * each member of the outer collection gets a different self-stack head
    * at runtime, so we can't bake the outer Factual in at build time. */
  def apply(path: Path)(using fact: Factual): CompNode =
    val (startFact, resolvedPath) = path.popEscapes(fact, fact.selfStack)
    startFact(resolvedPath)(0) match
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
