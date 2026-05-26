package gov.irs.factgraph.compnodes

import gov.irs.factgraph.{ Expression, FactDictionary, Factual, Path, PathItem, WithSelfStack }
import gov.irs.factgraph.definitions.fact.{ CommonOptionConfigTraits, CompNodeConfigTrait }
import gov.irs.factgraph.monads.*
import gov.irs.factgraph.operators.CollectOperator
import gov.irs.factgraph.types.{ Collection, CollectionItem }

object Filter extends CompNodeFactory:
  override val Key: String = "Filter"

  private val operator = FilterOperator()

  def apply(path: Path, cnBuilder: Factual ?=> BooleanNode)(using
      fact: Factual,
  ): CompNode =
    fact(path :+ PathItem.Wildcard)(0) match
      case Result.Complete(collectionItem) =>
        // Pick the right Factual to push onto the SelfStack so `^` inside
        // the predicate resolves to the active scope just outside this
        // Filter:
        //   - Top-level Filter (fact is a FactDefinition whose `value` is
        //     mid-init): we can't push `fact` itself — `^/x` would
        //     re-enter its own `lazy val`. Push the fact's parent (the
        //     surrounding collection-item view, e.g. /members/*).
        //   - Nested Filter (fact is already a WithSelfStack from the
        //     outer Filter): push the wrapper itself so `^` resolves to
        //     the outer Filter's iterated item, not the outer
        //     iteration's parent collection.
        val outerScope: Factual = fact match
          case _: WithSelfStack => fact
          case _                =>
            fact(PathItem.Parent)(0) match
              case Result.Complete(p) => p
              case _                  => fact
        val innerCtx = WithSelfStack.push(collectionItem, outerScope)
        CollectionNode(
          Expression
            .Collect(path, cnBuilder(using innerCtx).expr, operator),
          Some(path),
        )
      case _ =>
        throw new IllegalArgumentException(
          s"cannot find fact at path '$path' from '${fact.path}'",
        )

  override def fromDerivedConfig(e: CompNodeConfigTrait)(using
      Factual,
  )(using
      FactDictionary,
  ): CompNode =
    val cnBuilder: Factual ?=> BooleanNode =
      CompNode.getConfigChildNode(e) match
        case node: BooleanNode => node
        case _                 =>
          throw new UnsupportedOperationException(
            s"invalid child type: $e",
          )

    this(Path(e.getOptionValue(CommonOptionConfigTraits.PATH).get), cnBuilder)

final private class FilterOperator extends CollectOperator[Collection, Boolean]:
  override def apply(
      vect: MaybeVector[(CollectionItem, Thunk[Result[Boolean]])],
  ): Result[Collection] =
    val (items, thunks) = vect.toVector.unzip
    val results = thunks.map(_.get)

    val bools = results.map(_.getOrElse(false))
    val filteredIds = for {
      i <- bools.indices if bools(i)
    } yield items(i).id

    val complete = vect.complete && results.forall(_.complete)

    Result(Collection(filteredIds.toVector), complete)
