package gov.irs.factgraph.compnodes

import gov.irs.factgraph.*
import gov.irs.factgraph.definitions.fact.{ CompNodeConfigElement, FactConfigElement, WritableConfigElement }
import gov.irs.factgraph.monads.Result
import gov.irs.factgraph.types.Collection
import java.util.UUID
import org.scalatest.funspec.AnyFunSpec

class EscapeFilterSpec extends AnyFunSpec:
  describe("Escape (`^`) path token") {
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
      val active = graph(Path(s"/members/#$memberActive/matchingIncomes"))(0).get
      assert(active.get(0) == Result.Complete(Collection(Vector(incomeOk))))

      val dormant = graph(Path(s"/members/#$memberDormant/matchingIncomes"))(0).get
      assert(dormant.get(0) == Result.Complete(Collection(Vector())))
    }
  }
