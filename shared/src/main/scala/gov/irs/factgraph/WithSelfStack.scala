package gov.irs.factgraph

import gov.irs.factgraph.compnodes.CompNode
import gov.irs.factgraph.limits.Limit
import gov.irs.factgraph.monads.*

/** Delegating Factual that carries a non-empty SelfStack.
  *
  * Filter (and any other compnode that re-contexts to a collection-item
  * before evaluating an inner predicate) wraps the inner item with this
  * wrapper, pushing the outer Factual onto the stack. Inner Dependency
  * builds and runtime evaluations see the wrapped Factual and can read
  * `fact.selfStack` to resolve `^`-prefixed paths.
  *
  * Every method other than `selfStack` delegates straight to `inner` —
  * the wrap is purely additive context.
  */
final class WithSelfStack(
    private val inner: Factual,
    override val selfStack: SelfStack,
) extends Factual:
  override def value: CompNode                            = inner.value
  override def path: Path                                 = inner.path
  override def meta: Factual.Meta                         = inner.meta
  override def size: Factual.Size                         = inner.size
  override def abstractPath: Path                         = inner.abstractPath
  override def limits: Seq[Limit]                         = inner.limits
  override def get: MaybeVector[Result[?]]                = inner.get
  override def getThunk: MaybeVector[Thunk[Result[?]]]    = inner.getThunk
  override def explain: MaybeVector[Explanation]          = inner.explain
  override def apply(p: Path): MaybeVector[Result[Factual]]    = inner(p)
  override def apply(k: PathItem): MaybeVector[Result[Factual]] = inner(k)

object WithSelfStack:
  /** Wrap `inner` so its `selfStack` is `outer :: inner.selfStack`. Composes
    * naturally for nested filters: a Filter inside a Filter inside a
    * per-member fact ends up with a 2-deep stack. */
  def push(inner: Factual, outer: Factual): Factual =
    new WithSelfStack(inner, inner.selfStack.push(outer))
