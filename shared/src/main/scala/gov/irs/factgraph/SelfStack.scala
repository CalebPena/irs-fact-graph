package gov.irs.factgraph

final case class SelfStack(stack: List[Factual]):
  def push(f: Factual): SelfStack = SelfStack(f :: stack)

  def pop(level: Int): Option[(Factual, SelfStack)] =
    if level >= 1 && stack.length >= level then
      Some((stack(level - 1), SelfStack(stack.drop(level))))
    else None

object SelfStack:
  val empty: SelfStack = SelfStack(Nil)
  given default: SelfStack = empty
