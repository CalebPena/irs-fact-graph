package gov.irs.factgraph

/** Stack of outer-collection-item Factuals exposed to nested filter predicates.
  *
  * When an expression evaluates inside `<Filter>` (or any compnode that
  * re-contexts to a per-item Factual), the surrounding Factual is no longer
  * the active "self" — the predicate is run with each candidate item as the
  * active Factual instead. That makes paths like `<Dependency path="memberId"/>`
  * resolve against the candidate, which is usually what you want.
  *
  * What you *can't* do without help is reach back out: e.g. inside a Filter
  * over `/incomes`, ask "give me the member this fact is being computed for."
  * The `^` (Escape) PathItem is the syntactic answer; SelfStack is the
  * machinery that makes it work.
  *
  * The stack head is the immediately-enclosing item; subsequent entries are
  * the further-out scopes (one entry per Filter/Find/IndexOf you crossed to
  * get to where you are now). `<Dependency path="^/foo"/>` consults
  * `selfStack.pop(1)` to start the rest of the path resolution from there.
  * `^^` is `selfStack.pop(2)`, and so on.
  *
  * Threaded at both construction and runtime via a `using SelfStack`
  * contextual parameter, with a default of `SelfStack.empty`.
  */
final case class SelfStack(stack: List[Factual]):
  def push(f: Factual): SelfStack = SelfStack(f :: stack)

  /** Returns the Factual `level` entries deep (1-indexed: level=1 is the
    * immediately-enclosing scope) along with the remainder of the stack
    * (everything above the popped entry, so further escapes can keep
    * climbing). None when the stack isn't deep enough.
    */
  def pop(level: Int): Option[(Factual, SelfStack)] =
    if level >= 1 && stack.length >= level then
      Some((stack(level - 1), SelfStack(stack.drop(level))))
    else None

object SelfStack:
  val empty: SelfStack = SelfStack(Nil)
  given default: SelfStack = empty
