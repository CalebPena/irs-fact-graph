package gov.irs.factgraph
import java.util.UUID
import upickle.default.ReadWriter

enum PathItem derives ReadWriter:
  case Child(key: Symbol)
  case Parent

  // Pop `level` filter scopes when resolving a path. Written as `^` for one
  // level, `^^` for two, etc. Each `^` jumps past one enclosing collection
  // operator (Filter/IndexOf/Find) so an inner predicate can reach back to
  // the surrounding collection-item that the outer fact is being evaluated
  // for. See SelfStack for the runtime/construction threading.
  case Escape(level: Int)

  // Collections
  case Member(id: UUID)
  case Wildcard
  case Unknown

  def isKnown: Boolean = this match
    case Child(_) | Member(_) => true
    case _                    => false

  def isAbstract: Boolean = this match
    case Child(_) | Wildcard => true
    case _                   => false

  def isWildcard: Boolean = this match
    case Wildcard => true
    case _        => false

  def isCollectionMember: Boolean = this match
    case Member(_) => true
    case _         => false

  def isEscape: Boolean = this match
    case Escape(_) => true
    case _         => false

  def asAbstract: PathItem = this match
    case Member(_) | Unknown => Wildcard
    case _                   => this

  override def toString: String = this match
    case Wildcard    => PathItem.WildcardKey
    case Unknown     => PathItem.UnknownKey
    case Parent      => PathItem.ParentKey
    case Escape(n)   => PathItem.EscapeKey.toString * n
    case Member(id)  => s"${PathItem.MemberPrefix}${id}"
    case Child(key)  => key.name

object PathItem:
  val WildcardKey = "*"
  private val UnknownKey = "?"
  private val ParentKey = ".."
  val EscapeKey: Char = '^'

  val MemberPrefix = '#'

  def apply(str: String): PathItem = str match
    case WildcardKey                                       => PathItem.Wildcard
    case UnknownKey                                        => PathItem.Unknown
    case ParentKey                                         => PathItem.Parent
    case _ if str.nonEmpty && str.forall(_ == EscapeKey)   =>
      PathItem.Escape(str.length)
    case _ if str.charAt(0) == MemberPrefix                =>
      PathItem.Member(UUID.fromString(str.substring(1)))
    case _ => PathItem.Child(Symbol(str))
