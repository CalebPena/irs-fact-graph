package gov.irs.factgraph

import gov.irs.factgraph.compnodes.CompNode
import gov.irs.factgraph.limits.Limit
import gov.irs.factgraph.monads.*

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
  def push(inner: Factual, outer: Factual): Factual =
    new WithSelfStack(inner, inner.selfStack.push(outer))
