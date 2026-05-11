package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ FactDictionary, Factual, Path }
import gov.irs.factgraph.definitions.fact.{ CommonOptionConfigTraits, CompNodeConfigTrait }
import gov.irs.factgraph.monads.Result

object Dependency extends CompNodeFactory:
  override val Key: String = "Dependency"

  def apply(path: Path)(using fact: Factual): CompNode =
    val (startFact, resolvedPath) = path.popEscapes(fact)
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
