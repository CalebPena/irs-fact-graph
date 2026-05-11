package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ FactDictionary, Factual, Path }
import gov.irs.factgraph.definitions.fact.{ CommonOptionConfigTraits, CompNodeConfigTrait }
import gov.irs.factgraph.monads.Result

object Dependency extends CompNodeFactory:
  override val Key: String = "Dependency"

  // The stored Expression.Dependency keeps the original `path` (escape items
  // and all) so runtime evaluation can re-resolve against whatever selfStack
  // is in scope when the Filter's predicate actually fires — each member of
  // the outer collection gets a different selfStack head at runtime.
  def apply(path: Path)(using fact: Factual): CompNode =
    val (startFact, resolvedPath) = path.popEscapes(fact)
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
