package gov.irs.factgraph.compnodes

import gov.irs.factgraph.*
import gov.irs.factgraph.definitions.fact.{ CompNodeConfigElement, FactConfigElement, WritableConfigElement }
import gov.irs.factgraph.monads.Result
import gov.irs.factgraph.types.Collection
import java.util.UUID
import org.scalatest.funspec.AnyFunSpec

/** Covers the `^` (Escape) PathItem — letting a Filter's predicate reach back
  * to the outer collection-item that the surrounding fact is being evaluated
  * for. Without `^` there's no way to express "for *this* member, filter
  * /incomes by memberId == self" because Filter's predicate context shadows
  * the outer Factual.
  */
class EscapeFilterSpec extends AnyFunSpec:
  describe("Escape (`^`) path token") {
    val dictionary = FactDictionary()

    // /members collection — each member has a boolean flag /members/*/active.
    // The per-member fact /members/*/anyMatchingIncome filters /incomes by
    // "active" — which is on the OUTER member, reached via `^/active`. This
    // exercises the construction-time and runtime self-stack threading.
    FactDefinition.fromConfig(
      FactConfigElement(
        "/members",
        Some(new WritableConfigElement("Collection")),
        None,
        None,
      ),
    )(using dictionary)

    FactDefinition.fromConfig(
      FactConfigElement(
        "/members/*/active",
        Some(new WritableConfigElement("Boolean")),
        None,
        None,
      ),
    )(using dictionary)

    FactDefinition.fromConfig(
      FactConfigElement(
        "/incomes",
        Some(new WritableConfigElement("Collection")),
        None,
        None,
      ),
    )(using dictionary)

    FactDefinition.fromConfig(
      FactConfigElement(
        "/incomes/*/eligible",
        Some(new WritableConfigElement("Boolean")),
        None,
        None,
      ),
    )(using dictionary)

    // For each member, return the collection of incomes whose "eligible"
    // flag matches *this member's* "active" flag. The predicate is
    // <Dependency path="../eligible"/> AND <Dependency path="^/active"/>
    // expressed via a single Filter over /incomes that reads `^/active`
    // (the outer member's flag) — only when the outer member is active
    // does any income pass.
    FactDefinition.fromConfig(
      FactConfigElement(
        "/members/*/matchingIncomes",
        None,
        Some(
          new CompNodeConfigElement(
            "Filter",
            Seq(
              new CompNodeConfigElement(
                "All",
                Seq(
                  new CompNodeConfigElement("Dependency", Seq.empty, "eligible"),
                  new CompNodeConfigElement("Dependency", Seq.empty, "^/active"),
                ),
              ),
            ),
            "/incomes",
          ),
        ),
        None,
      ),
    )(using dictionary)

    val graph = Graph(dictionary)

    val memberActive: UUID  = UUID.fromString("11111111-1111-1111-1111-111111111111")
    val memberDormant: UUID = UUID.fromString("22222222-2222-2222-2222-222222222222")
    val incomeOk: UUID      = UUID.fromString("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa")
    val incomeBad: UUID     = UUID.fromString("bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb")

    for {
      result <- graph(Path("/members"))
      fact   <- result
    } fact.set(Collection(Vector(memberActive, memberDormant)))

    for {
      result <- graph(Path("/incomes"))
      fact   <- result
    } fact.set(Collection(Vector(incomeOk, incomeBad)))

    // Save the collections before setting per-member/per-income fields so
    // the member/income facts resolve properly when we go to write them.
    graph.save()

    for {
      r <- graph(Path(s"/members/#$memberActive/active"))
      f <- r
    } f.set(true)
    for {
      r <- graph(Path(s"/members/#$memberDormant/active"))
      f <- r
    } f.set(false)
    for {
      r <- graph(Path(s"/incomes/#$incomeOk/eligible"))
      f <- r
    } f.set(true)
    for {
      r <- graph(Path(s"/incomes/#$incomeBad/eligible"))
      f <- r
    } f.set(false)
    graph.save()

    it("resolves to the outer collection-item from inside a Filter predicate") {
      // For the active member: outer active = true, so income.eligible
      // alone decides — only incomeOk passes.
      val active = graph(Path(s"/members/#$memberActive/matchingIncomes"))(0).get
      assert(active.get(0) == Result.Complete(Collection(Vector(incomeOk))))

      // For the dormant member: outer active = false, so the All gate
      // fails for every candidate income — empty filter result.
      val dormant = graph(Path(s"/members/#$memberDormant/matchingIncomes"))(0).get
      assert(dormant.get(0) == Result.Complete(Collection(Vector())))
    }
  }
