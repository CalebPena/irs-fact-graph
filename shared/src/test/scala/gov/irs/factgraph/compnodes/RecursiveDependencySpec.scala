package gov.irs.factgraph.compnodes

import gov.irs.factgraph.*
import gov.irs.factgraph.definitions.fact.{
  CompNodeConfigElement,
  FactConfigElement,
  WritableConfigElement,
}
import gov.irs.factgraph.monads.Result
import gov.irs.factgraph.types.Collection
import java.util.UUID
import org.scalatest.funspec.AnyFunSpec

class RecursiveDependencySpec extends AnyFunSpec:
  describe("Recursive Dependency") {
    val dictionary = FactDictionary()

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
        "/members/*/isHeadOfHousehold",
        Some(new WritableConfigElement("Boolean")),
        None,
        None,
      ),
    )(using dictionary)

    FactDefinition.fromConfig(
      FactConfigElement(
        "/members/*/relatedTo",
        Some(new WritableConfigElement("CollectionItem", "/members")),
        None,
        None,
      ),
    )(using dictionary)

    FactDefinition.fromConfig(
      FactConfigElement(
        "/members/*/isRelatedToHead",
        None,
        Some(
          new CompNodeConfigElement(
            "Any",
            Seq(
              new CompNodeConfigElement(
                "Dependency",
                Seq.empty,
                "../isHeadOfHousehold",
              ),
              new CompNodeConfigElement(
                "Dependency",
                Seq.empty,
                "../relatedTo/isHeadOfHousehold",
              ),
              new CompNodeConfigElement(
                "Dependency",
                Seq.empty,
                "../relatedTo/isRelatedToHead",
              ),
            ),
          ),
        ),
        None,
      ),
    )(using dictionary)

    val graph = Graph(dictionary)

    val head:  UUID = UUID.fromString("11111111-1111-1111-1111-111111111111")
    val child: UUID = UUID.fromString("22222222-2222-2222-2222-222222222222")
    val grand: UUID = UUID.fromString("33333333-3333-3333-3333-333333333333")
    val cycleA: UUID = UUID.fromString("44444444-4444-4444-4444-444444444444")
    val cycleB: UUID = UUID.fromString("55555555-5555-5555-5555-555555555555")
    val orphan: UUID = UUID.fromString("66666666-6666-6666-6666-666666666666")

    for {
      r <- graph(Path("/members"))
      f <- r
    } f.set(Collection(Vector(head, child, grand, cycleA, cycleB, orphan)))
    graph.save()

    for { r <- graph(Path(s"/members/#$head/isHeadOfHousehold")); f <- r } f.set(true)
    for { r <- graph(Path(s"/members/#$child/isHeadOfHousehold")); f <- r } f.set(false)
    for { r <- graph(Path(s"/members/#$grand/isHeadOfHousehold")); f <- r } f.set(false)
    for { r <- graph(Path(s"/members/#$cycleA/isHeadOfHousehold")); f <- r } f.set(false)
    for { r <- graph(Path(s"/members/#$cycleB/isHeadOfHousehold")); f <- r } f.set(false)
    for { r <- graph(Path(s"/members/#$orphan/isHeadOfHousehold")); f <- r } f.set(false)

    for { r <- graph(Path(s"/members/#$child/relatedTo")); f <- r } f.set(
      gov.irs.factgraph.types.CollectionItem(head),
    )
    for { r <- graph(Path(s"/members/#$grand/relatedTo")); f <- r } f.set(
      gov.irs.factgraph.types.CollectionItem(child),
    )
    for { r <- graph(Path(s"/members/#$cycleA/relatedTo")); f <- r } f.set(
      gov.irs.factgraph.types.CollectionItem(cycleB),
    )
    for { r <- graph(Path(s"/members/#$cycleB/relatedTo")); f <- r } f.set(
      gov.irs.factgraph.types.CollectionItem(cycleA),
    )
    graph.save()

    it("returns Complete(true) for the head itself") {
      val r = graph(Path(s"/members/#$head/isRelatedToHead"))(0).get
      assert(r.get(0) == Result.Complete(true))
    }

    it("returns Complete(true) for a member directly related to the head") {
      val r = graph(Path(s"/members/#$child/isRelatedToHead"))(0).get
      assert(r.get(0) == Result.Complete(true))
    }

    it("returns Complete(true) for a member transitively related to the head") {
      val r = graph(Path(s"/members/#$grand/isRelatedToHead"))(0).get
      assert(r.get(0) == Result.Complete(true))
    }

    it("returns Incomplete for members caught in a cycle (no head reachable)") {
      // Runtime cycle detection: A↔B with neither being head and no path
      // reaching the head. The runtime breaks the recursion and returns
      // Incomplete rather than stack-overflowing.
      val a = graph(Path(s"/members/#$cycleA/isRelatedToHead"))(0).get
      assert(a.get(0) == Result.Incomplete)
      val b = graph(Path(s"/members/#$cycleB/isRelatedToHead"))(0).get
      assert(b.get(0) == Result.Incomplete)
    }

    it("returns Incomplete for an orphan member with no relatedTo set") {
      val r = graph(Path(s"/members/#$orphan/isRelatedToHead"))(0).get
      assert(r.get(0) == Result.Incomplete)
    }
  }
