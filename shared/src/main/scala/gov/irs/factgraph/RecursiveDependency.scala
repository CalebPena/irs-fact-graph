package gov.irs.factgraph

import gov.irs.factgraph.compnodes.{
  BooleanNode,
  CompNode,
  DayNode,
  DollarNode,
  IntNode,
  RationalNode,
  StringNode,
}
import gov.irs.factgraph.definitions.fact.CompNodeConfigTrait

// Build-time support for derived facts that reference themselves through
// `<Dependency>`. The normal Dependency.apply flow resolves the target via
// `target.value` to learn its type — that re-enters the host FactDefinition's
// still-initializing `lazy val` and stack-overflows. We sidestep both
// problems by (1) walking the path through the dictionary's stored configs
// instead of through `value`, and (2) inferring the host's output type from
// its derived config's outer compnode without invoking the builder.
//
// Runtime cycle detection (in Graph + Fact.get) is separate; this file only
// handles the construction-time half.
object RecursiveDependency:

  // Resolve a relative Dependency path against the host FactDefinition's
  // abstract path, walking via the dictionary's stored FactConfigTrait and
  // following `<CollectionItem collection="…">` aliases by reading their
  // writable config (no FactDef.value access). Returns the resolved abstract
  // path when the walk succeeds, None otherwise.
  def resolveAbstractPath(
      host: FactDefinition,
      path: Path,
  ): Option[Path] =
    if (path.absolute)
      Some(path)
    else
      // Walk from host.path itself, matching runtime path-resolution
      // semantics: relative names look up children of the current fact,
      // and `..` walks parents. Authors reach siblings via `../sibling`.
      walk(host.path, path.items, host.dictionary)

  private def walk(
      current: Path,
      remaining: List[PathItem],
      dict: FactDictionary,
  ): Option[Path] = remaining match
    case Nil => Some(current)

    case PathItem.Parent :: rest =>
      current.parent.flatMap(walk(_, rest, dict))

    case PathItem.Child(_) :: rest =>
      val next = current :+ remaining.head
      // Following a `<CollectionItem collection="/X"/>` alias: jump to that
      // collection's item path before consuming the rest of the segments.
      val aliasJump = for
        fd     <- dict(next)
        writ   <- fd.config.flatMap(_.writable)
        if writ.typeName == "CollectionItem"
        alias  <- writ.collectionItemAlias
      yield Path(alias) :+ PathItem.Wildcard
      walk(aliasJump.getOrElse(next), rest, dict)

    case _ => None

  // Statically infer the CompNode output type of a derived config without
  // actually building it. Covers the always-Boolean comparison / boolean
  // ops, the literal and numeric-conversion nodes, and Switch (by walking
  // a non-self-referential Then branch). Returns the engine's writable
  // typeName (e.g. "Boolean", "Dollar") on success — None when the shape
  // is too ambiguous to decide statically.
  private val FixedTypes: Map[String, String] = Map(
    "All"                  -> "Boolean",
    "Any"                  -> "Boolean",
    "Not"                  -> "Boolean",
    "Equal"                -> "Boolean",
    "NotEqual"             -> "Boolean",
    "LessThan"             -> "Boolean",
    "LessThanOrEqual"      -> "Boolean",
    "GreaterThan"          -> "Boolean",
    "GreaterThanOrEqual"   -> "Boolean",
    "IsComplete"           -> "Boolean",
    "EnumOptionsContains"  -> "Boolean",
    "True"                 -> "Boolean",
    "False"                -> "Boolean",
    "Boolean"              -> "Boolean",
    "Int"                  -> "Int",
    "Length"               -> "Int",
    "Count"                -> "Int",
    "CollectionSize"       -> "Int",
    "IndexOf"              -> "Int",
    "RoundToInt"           -> "Int",
    "Dollar"               -> "Dollar",
    "Day"                  -> "Day",
    "Today"                -> "Day",
    "AddPayrollMonths"     -> "Day",
    "LastDayOfMonth"       -> "Day",
    "Rational"             -> "Rational",
    "TruncateCents"        -> "Rational",
    "String"               -> "String",
    "AsString"             -> "String",
    "AsDecimalString"      -> "String",
    "ToUpper"              -> "String",
  )

  def inferType(host: FactDefinition): Option[String] =
    for
      cfg  <- host.config
      // Writable facts have a definite type from their writable config —
      // they shouldn't really show up as Dependency targets during build,
      // but handle them for symmetry.
      tpe  <- cfg.writable.map(_.typeName).orElse(cfg.derived.flatMap(inferDerivedType))
    yield tpe

  private def inferDerivedType(
      cfg: CompNodeConfigTrait,
  ): Option[String] =
    FixedTypes.get(cfg.typeName).orElse {
      cfg.typeName match
        case "Switch" =>
          // Pick the first Case's Then branch and recurse into its first
          // non-Dependency child. Dependency children would re-enter type
          // inference; for the recursive case we know they share the
          // Switch's overall type so any sibling branch works.
          cfg.children.iterator
            .filter(_.typeName == "Case")
            .flatMap(_.children.iterator)
            .filter(_.typeName == "Then")
            .flatMap(_.children.iterator)
            .find(_.typeName != "Dependency")
            .flatMap(inferDerivedType)
        case _ => None
    }

  // Build a typed CompNode wrapping `Expression.Dependency(path)`. Returns
  // None when we don't have a CompNode constructor registered for the
  // inferred typeName.
  def stubDependency(typeName: String, path: Path): Option[CompNode] =
    typeName match
      case "Boolean"  => Some(BooleanNode(Expression.Dependency(path)))
      case "Int"      => Some(IntNode(Expression.Dependency(path)))
      case "Dollar"   => Some(DollarNode(Expression.Dependency(path)))
      case "Day"      => Some(DayNode(Expression.Dependency(path)))
      case "Rational" => Some(RationalNode(Expression.Dependency(path)))
      case "String"   => Some(StringNode(Expression.Dependency(path)))
      case _          => None
