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
import gov.irs.factgraph.definitions.fact.{
  CommonOptionConfigTraits,
  CompNodeConfigTrait,
}

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
      tpe  <- cfg.writable
        .map(_.typeName)
        .orElse(
          cfg.derived.flatMap(d =>
            inferDerivedType(d, host, host.dictionary, Set(host.path)),
          ),
        )
    yield tpe

  // Shape-preserving numeric ops: output type matches a numeric child. The
  // immediate children may be wrapped in named tags (Subtract uses Minuend
  // / Subtrahends, Divide uses Dividend / Divisors); for those we flatten
  // one extra level when iterating.
  private val NumericPropagators: Set[String] = Set(
    "Add", "Subtract", "Multiply", "Divide",
    "GreaterOf", "LesserOf", "Minimum", "Maximum", "Min", "Max",
    "Round", "Floor", "Ceiling", "PayrollMonthsBetween",
  )
  private val WrapperTagNames: Set[String] = Set(
    "Minuend", "Subtrahends", "Dividend", "Divisors", "Multiplicand",
    "Left", "Right",
  )

  private def inferDerivedType(
      cfg: CompNodeConfigTrait,
      host: FactDefinition,
      dict: FactDictionary,
      visited: Set[Path],
  ): Option[String] =
    FixedTypes.get(cfg.typeName).orElse {
      cfg.typeName match
        case "Switch" =>
          // Try every Then branch's first child until one yields a type.
          cfg.children.iterator
            .filter(_.typeName == "Case")
            .flatMap(_.children.iterator)
            .filter(_.typeName == "Then")
            .flatMap(_.children.iterator)
            .map(child => inferDerivedType(child, host, dict, visited))
            .collectFirst { case Some(t) => t }

        case t if NumericPropagators.contains(t) =>
          // Type comes from children. Walk past any wrapper tags
          // (Minuend, Dividend, …) and try every direct child until one
          // resolves to a concrete type.
          flattenChildren(cfg).iterator
            .map(child => inferDerivedType(child, host, dict, visited))
            .collectFirst { case Some(t2) => t2 }

        case "Dependency" =>
          // Resolve target via the dictionary and recurse. Skip targets
          // we're already inferring (cycle in the type-inference walk
          // back to the host or another fact mid-resolution).
          for
            rawPath <- cfg.getOptionValue(CommonOptionConfigTraits.PATH)
            resolved <- resolveAbstractPath(host, Path(rawPath))
            if !visited.contains(resolved)
            target <- dict(resolved)
            tcfg <- target.config
            tpe <- tcfg.writable
              .map(_.typeName)
              .orElse(
                tcfg.derived.flatMap(d =>
                  inferDerivedType(d, target, dict, visited + resolved),
                ),
              )
          yield tpe

        case _ => None
    }

  private def flattenChildren(
      cfg: CompNodeConfigTrait,
  ): Iterable[CompNodeConfigTrait] =
    cfg.children.flatMap(c =>
      if (WrapperTagNames.contains(c.typeName)) c.children
      else Seq(c),
    )

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
